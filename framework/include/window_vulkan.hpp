#pragma once

namespace cgb
{
	class window : public window_base
	{
		friend class generic_glfw;
		friend class vulkan;
	public:

		window() = default;
		window(window&&) noexcept = default;
		window(const window&) = delete;
		window& operator =(window&&) noexcept = default;
		window& operator =(const window&) = delete;
		~window() = default;

		/** Request a framebuffer for this window which is capable of sRGB formats */
		void request_srgb_framebuffer(bool aRequestSrgb);

		/** Sets the presentation mode for this window's swap chain. */
		void set_presentaton_mode(cgb::presentation_mode aMode);

		/** Sets the number of samples for MSAA */
		void set_number_of_samples(int aNumSamples);

		/** Sets the number of presentable images for a swap chain */
		void set_number_of_presentable_images(uint32_t aNumImages);

		/** Sets the number of images which can be rendered into concurrently,
		 *	i.e. the number of "frames in flight"
		 */
		void set_number_of_concurrent_frames(uint32_t aNumConcurrent);

		/** Sets additional attachments which shall be added to the back buffer 
		 *	in addition to the obligatory color attachment.  
		 */
		void set_additional_back_buffer_attachments(std::vector<attachment> aAdditionalAttachments);

		/** Creates or opens the window */
		void open();

		/** Gets the requested surface format for the given surface.
		 *	A default value will be set if no other value has been configured.
		 */
		vk::SurfaceFormatKHR get_config_surface_format(const vk::SurfaceKHR& aSurface);

		/** Gets the requested presentation mode for the given surface.
		 *	A default value will be set if no other value has been configured.
		 */
		vk::PresentModeKHR get_config_presentation_mode(const vk::SurfaceKHR& aSurface);

		/**	Gets the number of samples that has been configured.
		 *	A default value will be set if no other value has been configured.
		 */
		vk::SampleCountFlagBits get_config_number_of_samples();

		/** Gets the multisampling-related config info struct for the Vk-pipeline config.
		 *	A default value will be set if no other value has been configured.
		 */
		vk::PipelineMultisampleStateCreateInfo get_config_multisample_state_create_info();

		/** Get the minimum number of concurrent/presentable images for a swap chain.
		*	If no value is set, the surfaces minimum number + 1 will be returned.
		*/
		uint32_t get_config_number_of_presentable_images();

		/** Get the number of concurrent frames.
		*	If no value is explicitely set, the same number as the number of presentable images will be returned.
		*/
		uint32_t get_config_number_of_concurrent_frames();

		/**	Gets the descriptions of the additional back buffer attachments
		 */
		std::vector<attachment> get_additional_back_buffer_attachments();

		/** Gets this window's surface */
		const auto& surface() const { 
			return mSurface.get(); 
		}
		/** Gets this window's swap chain */
		const auto& swap_chain() const { 
			return mSwapChain.get(); 
		}
		/** Gets this window's swap chain's image format */
		const auto& swap_chain_image_format() const { 
			return mSwapChainImageFormat; 
		}
		/** Gets this window's swap chain's dimensions */
		auto swap_chain_extent() const {
			return mSwapChainExtent; 
		}
		/** Gets a collection containing all this window's swap chain images. */
		const auto& swap_chain_images() { 
			return mSwapChainImages;
		}
		/** Gets this window's swap chain's image at the specified index. */
		const auto& swap_chain_image_at_index(size_t aIdx) { 
			return mSwapChainImages[aIdx]; 
		}
		/** Gets a collection containing all this window's swap chain image views. */
		const auto& swap_chain_image_views() { 
			return mSwapChainImageViews; 
		}
		/** Gets this window's swap chain's image view at the specified index. */
		const auto& swap_chain_image_view_at_index(size_t aIdx) { 
			return mSwapChainImageViews[aIdx]; 
		}

		/** Gets a collection containing all this window's back buffers. */
		const auto& backbuffers() { 
			return mBackBuffers; 
		}
		/** Gets this window's back buffer at the specified index. */
		const auto& backbuffer_at_index(size_t aIdx) { 
			return mBackBuffers[aIdx]; 
		}

		/** Gets the number of how many frames are (potentially) concurrently rendered into,
		 *	or put differently: How many frames are (potentially) "in flight" at the same time.
		 */
		auto number_of_in_flight_frames() const { 
			return static_cast<int64_t>(mFences.size());
		}

		/** Gets the current frame index. */
		auto current_frame() const { 
			return mCurrentFrame; 
		}

		/** Returns the "in flight index" for the requested frame.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		auto in_flight_index_for_frame(std::optional<int64_t> aFrameId = {}) const { 
			return aFrameId.value_or(current_frame()) % number_of_in_flight_frames(); 
		}
		
		/** Returns the swap chain image for the requested frame, which depends on the frame's "in flight index.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		const auto& image_for_frame(std::optional<int64_t> aFrameId = {}) const {
			return mSwapChainImages[in_flight_index_for_frame(aFrameId)];
		}
		/** Returns the swap chain image view for the requested frame, which depends on the frame's "in flight index.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		const image_view_t& image_view_for_frame(std::optional<int64_t> aFrameId = {}) const {
			return mSwapChainImageViews[in_flight_index_for_frame(aFrameId)];
		}
		/** Returns the fence for the requested frame, which depends on the frame's "in flight index.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		const fence_t& fence_for_frame(std::optional<int64_t> aFrameId = {}) const {
			return mFences[in_flight_index_for_frame(aFrameId)];
		}
		/** Returns the "image available"-semaphore for the requested frame, which depends on the frame's "in flight index.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		const semaphore_t& image_available_semaphore_for_frame(std::optional<int64_t> aFrameId = {}) const {
			return mImageAvailableSemaphores[in_flight_index_for_frame(aFrameId)];
		}
		/** Returns the "render finished"-semaphore for the requested frame, which depends on the frame's "in flight index.
		 *	@param aFrameId		If set, refers to the absolute frame-id of a specific frame.
		 *						If not set, refers to the current frame, i.e. `current_frame()`.
		 */
		const auto& render_finished_semaphore_for_frame(std::optional<int64_t> aFrameId = {}) const {
			return mRenderFinishedSemaphores[in_flight_index_for_frame(aFrameId)];
		}

		/**	Add an extra semaphore to wait on for the given frame id.
		 *	@param	aSemaphore		The semaphore to take ownership for and to set as dependency for a (future) frame.
		 *	@param	aFrameId		The (future) frame-id which this semaphore shall be a dependency for.
		 *							If the parameter is not set, the semaphore will be assigned to the current_frame()-id,
		 *							which means for the next frame which will be rendered. The next frame which will be 
		 *							rendered is the frame with the id current_frame(), assuming this function is called 
		 *							before render_frame() is called.
		 */
		void set_extra_semaphore_dependency(semaphore aSemaphore, std::optional<int64_t> aFrameId = {});

		/**	Pass a "single use" command buffer for the given frame and have its lifetime handled.
		 *	The lifetime of this command buffer will last until the given frame + number of frames in flight.
		 *	@param	aCommandBuffer	The command buffer to take ownership of and to handle lifetime of.
		 *	@param	aFrameId		The frame this command buffer is associated to.
		 */
		void handle_single_use_command_buffer_lifetime(command_buffer aCommandBuffer, std::optional<int64_t> aFrameId = {});

		std::vector<semaphore> remove_all_extra_semaphore_dependencies_for_frame(int64_t aFrameId);

		/** Remove all the "single use" command buffers for the given frame.
		 *	The command buffers are moved out of the internal storage and returned to the caller.
		 */
		std::vector<command_buffer> remove_all_single_use_command_buffers_for_frame(int64_t aFrameId);

		void fill_in_extra_semaphore_dependencies_for_frame(std::vector<vk::Semaphore>& aSemaphores, int64_t aFrameId);

		void fill_in_extra_render_finished_semaphores_for_frame(std::vector<vk::Semaphore>& aSemaphores, int64_t aFrameId);

		//std::vector<semaphore> set_num_extra_semaphores_to_generate_per_frame(uint32_t _NumExtraSemaphores);

		//template<typename CBT, typename... CBTS>
		//void render_frame(CBT _CommandBuffer, CBTS... _CommandBuffers)
		void render_frame(std::vector<std::reference_wrapper<const cgb::command_buffer_t>> aCommandBufferRefs, std::optional<std::reference_wrapper<cgb::image_t>> aCopyToPresent = {});

		const auto& renderpass_handle() const { return (*mBackBufferRenderpass).handle(); }

		auto& getrenderpass() const { return mBackBufferRenderpass; }



	protected:
		

#pragma region configuration properties
		// A function which returns the surface format for this window's surface
		unique_function<vk::SurfaceFormatKHR(const vk::SurfaceKHR&)> mSurfaceFormatSelector;

		// A function which returns the desired presentation mode for this window's surface
		unique_function<vk::PresentModeKHR(const vk::SurfaceKHR&)> mPresentationModeSelector;

		// A function which returns the MSAA sample count for this window's surface
		unique_function<vk::SampleCountFlagBits()> mNumberOfSamplesGetter;

		// A function which returns the MSAA state for this window's surface
		unique_function<vk::PipelineMultisampleStateCreateInfo()> mMultisampleCreateInfoBuilder;

		// A function which returns the desired number of presentable images in the swap chain
		unique_function<uint32_t()> mNumberOfPresentableImagesGetter;

		// A function which returns the number of images which can be rendered into concurrently
		// According to this number, the number of semaphores and fences will be determined.
		unique_function<uint32_t()> mNumberOfConcurrentFramesGetter;

		// A function which returns attachments which shall be attached to the back buffer
		// in addition to the obligatory color attachment.
		unique_function<std::vector<attachment>()> mAdditionalBackBufferAttachmentsGetter;
#pragma endregion

#pragma region swap chain data for this window surface
		// The frame counter/frame id/frame index/current frame number
		int64_t mCurrentFrame;

		// The window's surface
		vk::UniqueSurfaceKHR mSurface;
		// The swap chain for this surface
		vk::UniqueSwapchainKHR mSwapChain; 
		// The swap chain's image format
		image_format mSwapChainImageFormat;
		// The swap chain's extent
		vk::Extent2D mSwapChainExtent;
		// Queue family indices which have shared ownership of the swap chain images
		std::vector<uint32_t> mQueueFamilyIndices;
		// Image data of the swap chain images
		vk::ImageCreateInfo mImageCreateInfoSwapChain;
		// All the images of the swap chain
		std::vector<image_t> mSwapChainImages; // They don't need to be destroyed explicitely (due to get...()), ... 
		// All the image views of the swap chain
		std::vector<image_view> mSwapChainImageViews; // ...but the image views do!
#pragma endregion

#pragma region indispensable sync elements
		// Fences to synchronize between frames (CPU-GPU synchronization)
		std::vector<fence> mFences; 
		// Semaphores to wait for an image to become available (GPU-GPU synchronization) // TODO: true?
		std::vector<semaphore> mImageAvailableSemaphores; 
		// Semaphores to wait for rendering to finish (GPU-GPU synchronization) // TODO: true?
		std::vector<semaphore> mRenderFinishedSemaphores; 
#pragma endregion

#pragma region extra sync elements, i.e. exta semaphores
		// Extra semaphores for frames.
		// The first element in the tuple refers to the frame id which is affected.
		// The second element in the is the semaphore to wait on.
		// Extra dependency semaphores will be waited on along with the mImageAvailableSemaphores
		std::vector<std::tuple<int64_t, semaphore>> mExtraSemaphoreDependencies;
		 
		// Number of extra semaphores to generate per frame upon fininshing the rendering of a frame
		uint32_t mNumExtraRenderFinishedSemaphoresPerFrame;

		// Contains the extra semaphores to be signalled per frame
		// The length of this vector will be: number_of_concurrent_frames() * mNumExtraSemaphoresPerFrame
		// These semaphores will be signalled together with the mRenderFinishedSemaphores
		std::vector<semaphore> mExtraRenderFinishedSemaphores;
#pragma endregion

		// The renderpass used for the back buffers
		renderpass mBackBufferRenderpass;

		// The backbuffers of this window
		std::vector<framebuffer> mBackBuffers;

		// The render pass for this window's UI calls
		vk::RenderPass mUiRenderPass;

		// Command buffers which are only submitted once and only once - i.e., for a specific frame,
		// taking their ownership, handling their lifetime (which lasts until current frame + frames in flight)
		std::vector<std::tuple<int64_t, command_buffer>> mSingleUseCommandBuffers;
	};
}
