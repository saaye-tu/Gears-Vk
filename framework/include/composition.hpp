#pragma once

namespace cgb
{
	/**	A composition brings together all of the separate components, which there are
	 *	 - A timer
	 *	 - One or more windows
	 *	 - One or more @ref cg_element(-derived objects)
	 *	 
	 *	Upon @ref start, a composition spins up the game-/rendering-loop in which
	 *	all of the @ref cg_element's methods are called.
	 *	
	 *	A composition will internally call @ref set_global_composition_data in order
	 *	to make itself the currently active composition. By design, there can only 
	 *	be one active composition_interface at at time. You can think of a composition
	 *	being something like a scene, of which typically one can be active at any 
	 *	given point in time.
	 *	
	 *	\remark You don't HAVE to use this composition-class, if you are developing 
	 *	an alternative composition class or a different approach and still want to use
	 *	a similar structure as proposed by this composition-class, please make sure 
	 *	to call @ref set_global_composition_data 
	 */
	template <typename TTimer, typename TExecutor>
	class composition : public composition_interface
	{
	public:
		composition() :
			mTimer{},
			mExecutor{this},
			mInputBuffers{},
			mVaryingInputForegroundIndex{0},
			mVaryingInputBackgroundIndex{1},
			mFixedInputForegroundIndex	{2},
			mFixedInputBackgroundIndex	{3},
			mUserInputBufferIndex		{0},
			mShouldStop{false},
			mBackBufferActionsPending{false},
			mIsRunning{false}
		{
		}

		composition(std::initializer_list<cg_element*> pObjects) :
			mTimer{},
			mExecutor{this},
			mInputBuffers{},
			mVaryingInputForegroundIndex{0},
			mVaryingInputBackgroundIndex{1},
			mFixedInputForegroundIndex	{2},
			mFixedInputBackgroundIndex	{3},
			mUserInputBufferIndex		{0},
			mShouldStop{false},
			mBackBufferActionsPending{false},
			mIsRunning{false}
		{
			for (auto* el : pObjects) {
				auto it = std::lower_bound(std::begin(mElements), std::end(mElements), el, [](const cg_element* left, const cg_element* right) { return left->execution_order() < right->execution_order(); });
				mElements.insert(it, el);
			}
		}

		/** Provides access to the timer which is used by this composition */
		timer_interface& time() override
		{
			return mTimer;
		}

		/** Provides to the currently active input buffer, which contains the
		 *	current user input data */
		input_buffer& input() override
		{
			return mInputBuffers[mUserInputBufferIndex];
		}

		input_buffer& foreground_input_buffer_varying() override
		{
			return mInputBuffers[mVaryingInputForegroundIndex];
		}
		
		input_buffer& background_input_buffer_varying() override
		{
			return mInputBuffers[mVaryingInputBackgroundIndex];
		}

		input_buffer& foreground_input_buffer_fixed() override
		{
			return mInputBuffers[mFixedInputForegroundIndex];
		}

		input_buffer& background_input_buffer_fixed() override
		{
			return mInputBuffers[mFixedInputBackgroundIndex];
		}

		std::mutex& input_mutex() override
		{
			return sCompMutex;
		}

		/** Returns the @ref cg_element at the given index */
		cg_element* element_at_index(size_t pIndex) override
		{
			if (pIndex < mElements.size()) {
				return mElements[pIndex];
			}
			return nullptr;
		}

		/** Finds a @ref cg_element by its name 
		 *	\returns The element found or nullptr
		 */
		cg_element* element_by_name(std::string_view pName) override
		{
			auto found = std::find_if(
				std::begin(mElements), 
				std::end(mElements), 
				[&pName](const cg_element* element)
				{
					return element->name() == pName;
				});

			if (found != mElements.end())
				return *found;

			return nullptr;
		}

		/** Finds the @ref cg_element(s) with matching type.
		 *	@param pType	The type to look for
		 *	@param pIndex	Use this parameter to get the n-th element of the given type
		 *	\returns An element of the given type or nullptr
		 */
		cg_element* element_by_type(const std::type_info& pType, uint32_t pIndex) override
		{
			uint32_t nth = 0;
			for (auto* element : mElements)
			{
				if (typeid(element) == pType)
				{
					if (pIndex == nth++)
					{
						return element;
					}
				}
			}
			return nullptr;
		}

	private:
		/** Add all elements which are about to be added to the composition */
		void add_pending_elements()
		{
			// Make a copy of all the elements to be added to not interfere with erase-operations:
			std::unique_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			auto toBeAdded = mElementsToBeAdded;
			guard.unlock();
			for (auto el : toBeAdded) {
				add_element_immediately(*el);
			}
		}

		/** Remove all elements which are about to be removed */
		void remove_pending_elements()
		{
			// Make a copy of all the elements to be added to not interfere with erase-operations:
			std::unique_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			auto toBeRemoved = mElementsToBeRemoved;
			guard.unlock();
			for (auto el : toBeRemoved) {
				remove_element_immediately(*el);
			}
			assert(mElementsToBeRemoved.size() == 0);
		}

		/** Signal the main thread to start working off back buffer actions */
		static void back_buffer_actions_good_to_go(composition* thiz)
		{
			assert(false == thiz->mBackBufferActionsPending);
			thiz->mBackBufferActionsPending = true;
			context().signal_waiting_main_thread();
		}

		/** Wait (on the rendering thread) until the main thread has worked off back buffer actions */
		static void wait_for_back_buffer_actions_completed(composition* thiz)
		{
			using namespace std::chrono_literals;
			
			std::unique_lock<std::mutex> lk(sCompMutex);
			if (thiz->mBackBufferActionsPending) {
#if defined(_DEBUG)
				auto totalWaitTime = 0ms;
				auto timeout = 1ms;
				int cnt = 0;
				while (!sInputBufferCondVar.wait_for(lk, timeout, [thiz]{ return !thiz->mBackBufferActionsPending; })) {
					cnt++;
					totalWaitTime += timeout;
					LOG_DEBUG(fmt::format("Condition variable waits for timed out. Total wait time in current frame is {}", totalWaitTime));	
				}
#else
				sInputBufferCondVar.wait(lk, [thiz]{ return !thiz->mBackBufferActionsPending; });
#endif
			}
			assert(thiz->mBackBufferActionsPending == false);
		}

		/** Rendering thread's main function */
		static void render_thread(composition* thiz)
		{
			// Used to distinguish between "simulation" and "render"-frames
			auto frameType = timer_frame_type::none;

			while (!thiz->mShouldStop)
			{
				context().signal_waiting_main_thread(); // Let the main thread do some work in the meantime

				// Signal context
				context().begin_frame();

				// Add pending elements
				thiz->add_pending_elements();

				// 2. check and possibly issue on_enable event handlers
				thiz->mExecutor.execute_handle_enablings(thiz->mElements);

				// Prepare for input handling and TICK
				auto* windowForCursorActions = context().window_in_focus();
				frameType = thiz->mTimer.tick();

				wait_for_back_buffer_actions_completed(thiz);

				if (timer_frame_type::fixed == frameType) // ATTENTION: Only true if the frameType is fixed ONLY!
				{
					// We need the fixed input immediately
					{
						std::scoped_lock<std::mutex> guard(thiz->input_mutex());
						input_buffer::prepare_for_next_frame(
							thiz->background_input_buffer_fixed(),
							thiz->foreground_input_buffer_fixed(),
							windowForCursorActions
						);
						std::swap(
							thiz->mFixedInputBackgroundIndex, 
							thiz->mFixedInputForegroundIndex
						);
					}

					thiz->mUserInputBufferIndex = thiz->mFixedInputForegroundIndex;
					
					// 3. fixed update
					thiz->mExecutor.execute_fixed_updates(thiz->mElements);

					// signal context
					context().update_stage_done();
					context().signal_waiting_main_thread(); // Let the main thread work concurrently

					back_buffer_actions_good_to_go(thiz);
				}

				if ((frameType & timer_frame_type::varying) == timer_frame_type::varying)
				{
					// We need the varying input immediately
					{
						std::scoped_lock<std::mutex> guard(thiz->input_mutex());
						input_buffer::prepare_for_next_frame(
							thiz->background_input_buffer_varying(),
							thiz->foreground_input_buffer_varying(),
							windowForCursorActions
						);
						std::swap(
							thiz->mVaryingInputBackgroundIndex, 
							thiz->mVaryingInputForegroundIndex
						);
					}
					
					thiz->mUserInputBufferIndex = thiz->mVaryingInputForegroundIndex;

					// 3. fixed update
					if ((frameType & timer_frame_type::fixed) == timer_frame_type::fixed) {						
						thiz->mExecutor.execute_fixed_updates(thiz->mElements);
					}
					
					// 4. update
					windowForCursorActions->begin_frame(); // TODO: Do this for all windows? Or for those which were rendered into in the last frame?
					thiz->mExecutor.execute_updates(thiz->mElements);

					// signal context
					context().update_stage_done();
					context().signal_waiting_main_thread(); // Let the main thread work concurrently

					// Tell the main thread that we'd like to have the new input buffers from A) here:
					back_buffer_actions_good_to_go(thiz);

					// 5. render
					thiz->mExecutor.execute_renders(thiz->mElements);

					// 6. render_gizmos
					thiz->mExecutor.execute_render_gizmos(thiz->mElements);
					
					// 7. render_gui
					thiz->mExecutor.execute_render_guis(thiz->mElements);

					// Gather all the command buffers per window
					std::unordered_map<window*, std::vector<std::reference_wrapper<const cgb::command_buffer>>> toRender;
					for (auto& e : thiz->mElements)	{
						for (auto [cb, wnd] : e->mSubmittedCommandBufferReferences) {
							if (nullptr == wnd) {
								wnd = cgb::context().main_window();
							}
							toRender[wnd].push_back(cb);
						}
					}
					// Gather all the present images per window - there can only be max. one.
					std::unordered_map<window*, const cgb::image_t*> toPresent;
					for (auto& e : thiz->mElements)	{
						for (auto [img, wnd] : e->mPresentImages) {
							if (nullptr == wnd) {
								wnd = cgb::context().main_window();
							}
							if (toPresent.contains(wnd)) {
								LOG_WARNING(fmt::format("There is already an image designated to be presented for window [{}]. This call will have no effect.", fmt::ptr(wnd)));
							}
							else {
								toPresent[wnd] = &img.get();
							}
						}
					}
					// Render per window
					for (auto& [wnd, cbs] : toRender) {
						if (toPresent.contains(wnd)) {
							wnd->render_frame(std::move(cbs), std::cref(*toPresent[wnd]));
						}
						else {
							wnd->render_frame(std::move(cbs));
						}
					}
					// Transfer ownership of the command buffers in the elements' mSubmittedCommandBufferInstances to the respective window
					for (auto& e : thiz->mElements)	{
						for (auto& [cb, wnd] : e->mSubmittedCommandBufferInstances) {
							if (nullptr == wnd) {
								wnd = cgb::context().main_window();
							}
							wnd->set_one_time_submit_command_buffer(std::move(cb), wnd->current_frame() - 1);
						}
						// Also, cleanup the elements:
						e->mSubmittedCommandBufferReferences.clear();
						e->mSubmittedCommandBufferInstances.clear();
						// ...also the images to present:
						e->mPresentImages.clear();
					}
				}
				
				context().signal_waiting_main_thread(); // Let the main thread work concurrently

				// 8. check and possibly issue on_disable event handlers
				thiz->mExecutor.execute_handle_disablings(thiz->mElements);

				thiz->remove_pending_elements();
				
				// signal context
				context().end_frame();
			}

		}

	public:
		void add_element(cg_element& pElement) override
		{
			std::scoped_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			mElementsToBeAdded.push_back(&pElement);
		}

		void add_element_immediately(cg_element& pElement) override
		{
			std::scoped_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			// Find right place to insert:
			auto it = std::lower_bound(std::begin(mElements), std::end(mElements), &pElement, [](const cg_element* left, const cg_element* right) { return left->execution_order() < right->execution_order(); });
			mElements.insert(it, &pElement);
			// 1. initialize
			pElement.initialize();
			// Remove from mElementsToBeAdded container (if it was contained in it)
			mElementsToBeAdded.erase(std::remove(std::begin(mElementsToBeAdded), std::end(mElementsToBeAdded), &pElement), std::end(mElementsToBeAdded));
		}

		void remove_element(cg_element& pElement) override
		{
			std::scoped_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			mElementsToBeRemoved.push_back(&pElement);
		}

		void remove_element_immediately(cg_element& pElement, bool pIsBeingDestructed = false) override
		{
			std::scoped_lock<std::mutex> guard(sCompMutex); // For parallel executors, this is neccessary!
			if (!pIsBeingDestructed) {
				assert(std::find(std::begin(mElements), std::end(mElements), &pElement) != mElements.end());
				// 9. finalize
				pElement.finalize();
				// Remove from the actual elements-container
				mElements.erase(std::remove(std::begin(mElements), std::end(mElements), &pElement), std::end(mElements));
				// ...and from mElementsToBeRemoved
				mElementsToBeRemoved.erase(std::remove(std::begin(mElementsToBeRemoved), std::end(mElementsToBeRemoved), &pElement), std::end(mElementsToBeRemoved));
			}
			else {
				LOG_DEBUG(fmt::format("Removing element with name[{}] and address[{}] issued from cg_element's destructor",
										 pElement.name(),
										 fmt::ptr(&pElement)));
			}
		}

		void start() override
		{
			context().work_off_event_handlers();

			// Make myself the current composition_interface
			composition_interface::set_current(this);

			// 1. initialize
			for (auto& o : mElements)
			{
				o->initialize();
			}

			// Signal context after initialization
			context().begin_composition();
			context().work_off_event_handlers();

			// Enable receiving input
			auto windows_for_input = context().find_windows([](auto * w) { return w->is_input_enabled(); });
			for (auto* w : windows_for_input)
			{
				w->set_is_in_use(true);
				// Write into the buffer at mInputBufferUpdateIndex,
				// let client-objects read from the buffer at mInputBufferConsumerIndex
				context().start_receiving_input_from_window(*w);
				mWindowsReceivingInputFrom.push_back(w);
			}

			// game-/render-loop:
			mIsRunning = true;

			// off it goes
			std::thread renderThread(render_thread, this);
			
			while (!mShouldStop)
			{
				context().work_off_all_pending_main_thread_actions();
				context().work_off_event_handlers();

				std::unique_lock<std::mutex> lk(sCompMutex);
				auto* windowForCursorActions = context().window_in_focus();
				if (mBackBufferActionsPending) {

					input_buffer::work_off_back_buffer_actions(background_input_buffer_varying(),	windowForCursorActions);
					input_buffer::work_off_back_buffer_actions(background_input_buffer_fixed(),		windowForCursorActions);

					// reset flag:
					mBackBufferActionsPending = false;

					// resume render_thread:
					lk.unlock();
					sInputBufferCondVar.notify_one();
				}
				else {
					lk.unlock();
				}

				context().wait_for_input_events();
			}

			renderThread.join();

			mIsRunning = false;

			// Stop the input
			for (auto* w : mWindowsReceivingInputFrom)
			{
				context().stop_receiving_input_from_window(*w);
				w->set_is_in_use(false);
			}
			mWindowsReceivingInputFrom.clear();

			// Signal context before finalization
			context().end_composition();

			// 9. finalize
			for (auto& o : mElements)
			{
				o->finalize();
			}
		}

		/** Stop a currently running game/rendering-loop for this composition_interface */
		void stop() override
		{
			mShouldStop = true;
		}

		/** True if this composition_interface has been started but not yet stopped or finished. */
		bool is_running() override
		{
			return mIsRunning;
		}

	private:
		static std::mutex sCompMutex;
		std::atomic_bool mShouldStop;
		bool mBackBufferActionsPending;
		static std::condition_variable sInputBufferCondVar;

		bool mIsRunning;

		std::vector<window*> mWindowsReceivingInputFrom;
		std::vector<cg_element*> mElements;
		std::vector<cg_element*> mElementsToBeAdded;
		std::vector<cg_element*> mElementsToBeRemoved;
		TTimer mTimer;
		TExecutor mExecutor;
		std::array<input_buffer, 4> mInputBuffers;
		int32_t mVaryingInputForegroundIndex;
		int32_t mVaryingInputBackgroundIndex;
		int32_t mFixedInputForegroundIndex;
		int32_t mFixedInputBackgroundIndex;
		std::atomic_int_fast32_t mUserInputBufferIndex;
	};

	template <typename TTimer, typename TExecutor>
	std::mutex composition<TTimer, TExecutor>::sCompMutex{};

	template <typename TTimer, typename TExecutor>
	std::condition_variable composition<TTimer, TExecutor>::sInputBufferCondVar{};
		
}

