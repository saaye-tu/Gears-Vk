namespace cgb
{
	void input_buffer::reset(std::optional<window> pWindow) // TODO: Where is this used??
	{
		std::fill(std::begin(mKeyboardKeys), std::end(mKeyboardKeys), key_state::none);
		std::fill(std::begin(mMouseKeys), std::end(mMouseKeys), key_state::none);
		mWindow = nullptr;
		mCursorPosition = { 0.0, 0.0 };
		mDeltaCursorPosition = { 0.0, 0.0 };
		mScrollDelta = { 0.0, 0.0 };
		mCursorDisabled = false;
		mSetCursorPosition = std::nullopt;
		mSetCursorDisabled = std::nullopt;
	}

	void input_buffer::prepare_for_next_frame(input_buffer& pFrontBufferToBe, input_buffer& pBackBufferToBe, window* pWindow)
	{
		// Handle window changes (different window in focus) and other window-related actions
		pFrontBufferToBe.mWindow = pWindow;
		pFrontBufferToBe.mCursorDisabled = pFrontBufferToBe.mWindow->is_cursor_disabled();
		pFrontBufferToBe.mCursorPosition = pFrontBufferToBe.mWindow->cursor_position();
		if (pFrontBufferToBe.mWindow == pBackBufferToBe.mWindow && pBackBufferToBe.is_cursor_disabled() == pFrontBufferToBe.is_cursor_disabled()) {
			pFrontBufferToBe.mDeltaCursorPosition = pBackBufferToBe.mCursorPosition - pFrontBufferToBe.mCursorPosition;
		}
		else { // Window has changed!
			pFrontBufferToBe.mDeltaCursorPosition = { 0.0, 0.0 };
		}

		if (pBackBufferToBe.mSetCursorPosition.has_value()) {
			// Optimistic approach: Do not query GLFW for actual cursor position afterwards, just optimistically
			//						assume that the requested cursor position will match the real cursor position
			// TODO: The optimistic approach might be a problem.
			// BUT (important!), set both buffers to the center coordinates (because of the delta position)!
			pFrontBufferToBe.mCursorPosition	= pBackBufferToBe.mSetCursorPosition.value();
			pBackBufferToBe.mCursorPosition		= pBackBufferToBe.mSetCursorPosition.value();
		}

		{ // TODO: offload to a different thread.. but NOT the main thread! Deadlock danger! (generic_glfw might wait on a lock and never get it)
			auto& pPreviousBackBuffer = pFrontBufferToBe;
			auto& pCurrentBackBuffer = pBackBufferToBe;
			// Handle all the keyboard input
			for (size_t i = 0; i < pPreviousBackBuffer.mKeyboardKeys.size(); ++i) {
				// Retain those down-states:
				pCurrentBackBuffer.mKeyboardKeys[i] = (pPreviousBackBuffer.mKeyboardKeys[i] & key_state::down);
			}
			// Handle all the mouse button input
			for (size_t i = 0; i < pPreviousBackBuffer.mMouseKeys.size(); ++i) {
				// Retain those down-states:
				pCurrentBackBuffer.mMouseKeys[i] = (pPreviousBackBuffer.mMouseKeys[i] & key_state::down);
			}

			// Scroll delta is always a relative amount and filled into the back-buffer by the GLFW context,
			//  i.e. no need to alter it here, just reset it for the back-buffer.
			pCurrentBackBuffer.mScrollDelta = { 0.0, 0.0 };
		}
	}

	void input_buffer::work_off_back_buffer_actions(input_buffer& pCurrentBackBuffer, window* pWindow)
	{
		if (pCurrentBackBuffer.mSetCursorPosition.has_value()) {
			assert(context().are_we_on_the_main_thread());
			pWindow->set_cursor_pos({ pCurrentBackBuffer.mSetCursorPosition->x, pCurrentBackBuffer.mSetCursorPosition->y });
			// Mark action as done:
			pCurrentBackBuffer.mSetCursorPosition = std::nullopt;
		}

		if (pCurrentBackBuffer.mSetCursorDisabled.has_value()) {
			assert(context().are_we_on_the_main_thread());
			const bool toBeDisabled = pCurrentBackBuffer.mSetCursorDisabled.value();
			pWindow->disable_cursor(toBeDisabled);
			// Mark action as done:
			pCurrentBackBuffer.mSetCursorDisabled = std::nullopt;
		}
	}

	bool input_buffer::key_pressed(key_code pKey)
	{
		return (mKeyboardKeys[static_cast<size_t>(pKey)] & key_state::pressed) != key_state::none;
	}

	bool input_buffer::key_released(key_code pKey)
	{
		return (mKeyboardKeys[static_cast<size_t>(pKey)] & key_state::released) != key_state::none;
	}

	bool input_buffer::key_down(key_code pKey)
	{
		return (mKeyboardKeys[static_cast<size_t>(pKey)] & key_state::down) != key_state::none;
	}

	bool input_buffer::mouse_button_pressed(uint8_t pButtonIndex)
	{
		return (mMouseKeys[static_cast<size_t>(pButtonIndex)] & key_state::pressed) != key_state::none;
	}

	bool input_buffer::mouse_button_released(uint8_t pButtonIndex)
	{
		return (mMouseKeys[static_cast<size_t>(pButtonIndex)] & key_state::released) != key_state::none;
	}

	bool input_buffer::mouse_button_down(uint8_t pButtonIndex)
	{
		return (mMouseKeys[static_cast<size_t>(pButtonIndex)] & key_state::down) != key_state::none;
	}

	const glm::dvec2& input_buffer::cursor_position() const
	{
		return mCursorPosition;
	}

	const glm::dvec2& input_buffer::delta_cursor_position() const
	{
		return mDeltaCursorPosition;
	}

	const glm::dvec2& input_buffer::scroll_delta() const
	{
		return mScrollDelta;
	}

	void input_buffer::set_cursor_disabled(bool pDisabled)
	{
		mSetCursorDisabled = pDisabled;
	}

	bool input_buffer::is_cursor_disabled() const
	{
		return mCursorDisabled;
	}

	void input_buffer::set_cursor_position(glm::dvec2 pNewPosition)
	{
		mSetCursorPosition = pNewPosition;
	}
}