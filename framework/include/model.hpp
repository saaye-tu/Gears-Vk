#pragma once
#include <gvk.hpp>

namespace gvk
{
	using model_index_t = size_t;
	using mesh_index_t = size_t;

	struct animation_clip_data
	{
		unsigned int mAnimationIndex;
		double mTicksPerSecond;
		double mStartTicks;
		double mEndTicks;
	};

	struct animated_meshes
	{
		/** Collection of tuples with two elements:
		 *  [0]: The mesh index to be animated
		 *  [1]: Pointer to the target storage where bone matrices shall be written to
		 */
		std::vector<std::tuple<mesh_index_t, glm::mat4*>> mMeshIndicesAndTargetStorage;

		/** Maximum number of bone matrices to write into the target storage pointers.
		 *  (Second tuple element of mMeshIndicesAndTargetStorage)
		 */
		size_t mMaxNumBoneMatrices;

		// TMP:
		// Maps bone name with index
		std::vector<glm::mat4> m_boneFinalTransform;
		std::map<aiNode *, aiNodeAnim *> m_nodeToNodeAnimMap;
		std::map<std::string, uint32_t> m_boneNameIdMap;
		std::vector<glm::mat4> m_boneOffsetMatrix;
	};
	
	class model_t
	{
		friend class context_vulkan;
		
	public:
		using aiProcessFlagsType = unsigned int;

		model_t() = default;
		model_t(model_t&&) noexcept = default;
		model_t(const model_t&) = delete;
		model_t& operator=(model_t&&) noexcept = default;
		model_t& operator=(const model_t&) = delete;
		~model_t() = default;

		const auto* handle() const { return mScene; }

		static avk::owning_resource<model_t> load_from_file(const std::string& aPath, aiProcessFlagsType aAssimpFlags = aiProcess_Triangulate);
		
		static avk::owning_resource<model_t> load_from_memory(const std::string& aMemory, aiProcessFlagsType aAssimpFlags = aiProcess_Triangulate);

		/** Returns this model's path where it has been loaded from */
		auto path() const { return mModelPath; }

		/** Determine the transformation matrix for the mesh at the given index.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Transformation matrix of the given mesh, can be the identity
		 */
		glm::mat4 transformation_matrix_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets the name of the mesh at the given index (not to be confused with the material's name)
		 *	@param		_MeshIndex		The index corresponding to the mesh
		 *	@return		Mesh name converted from Assimp's internal representation to std::string
		 */
		std::string name_of_mesh(mesh_index_t _MeshIndex) const;

		/** Gets Assimp's internal material index for the given mesh index. 
		 *	This value won't be useful if not operating directly on Assimp's internal materials.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Mesh index corresponding to Assimp's internal materials structure.
		 */
		size_t material_index_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets the name of material at the given material index
		 *	@param		aMaterialIndex		The index corresponding to the material
		 *	@return		Material name converted from Assimp's internal representation to std::string
		 */
		std::string name_of_material(size_t aMaterialIndex) const;

		/** Gets the `material_config` struct for the mesh at the given index.
		 *	The `material_config` struct is created from Assimp's internal material data.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		`material_config` struct, representing the "type of material". 
		 *				To actually load all the resources it refers to, you'll have 
		 *				to create a `material` based on it.
		 */
		material_config material_config_for_mesh(mesh_index_t aMeshIndex);

		/**	Sets some material config struct for the mesh at the given index.
		 *	@param	aMeshIndex			The index corresponding to the mesh
		 *	@param	aMaterialConfig		Material config that is to be assigned to the mesh.
		 */
		void set_material_config_for_mesh(mesh_index_t aMeshIndex, const material_config& aMaterialConfig);

		/**	Gets all distinct `material_config` structs foor this model and, as a bonus, so to say,
		 *	also gets all the mesh indices which have the materials assigned to.
		 *	@param	aAlsoConsiderCpuOnlyDataForDistinctMaterials	Setting this parameter to `true` means that for determining if a material is unique or not,
		 *															also the data in the material struct are evaluated which only remain on the CPU. This CPU 
		 *															data will not be transmitted to the GPU. By default, this parameter is set to `false`, i.e.
		 *															only the GPU data of the `material_config` struct will be evaluated when determining the distinct
		 *															materials.
		 *															You'll want to set this parameter to `true` if you are planning to adapt your draw calls based
		 *															on one or all of the following `material_config` members: `mShadingModel`, `mWireframeMode`,
		 *															`mTwosided`, `mBlendMode`. If you don't plan to differentiate based on these, set to `false`.
		 *	@return	A `std::unordered_map` containing the distinct `material_config` structs as the
		 *			keys and a vector of mesh indices as the value type, i.e. `std::vector<size_t>`. 
		 */
		std::unordered_map<material_config, std::vector<mesh_index_t>> distinct_material_configs(bool aAlsoConsiderCpuOnlyDataForDistinctMaterials = false);
			
		/** Gets the number of vertices for the mesh at the given index.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Number of vertices, which is also the length of all the vectors,
		 *				which are returned by: `positions_for_mesh`, `normals_for_mesh`,
		 *				`tangents_for_mesh`, `bitangents_for_mesh`, `colors_for_mesh`, 
		 *				and `texture_coordinates_for_mesh`
		 */
		inline size_t number_of_vertices_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the positions for the mesh at the given index.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of vertex positions, converted to `glm::vec3` 
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec3> positions_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the normals for the mesh at the given index.
		 *	If the mesh has no normals, a vector filled with values is
		 *	returned regardless. All the values will be set to (0,0,1) in this case.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of normals, converted to `glm::vec3`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec3> normals_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the tangents for the mesh at the given index.
		 *	If the mesh has no tangents, a vector filled with values is
		 *	returned regardless. All the values will be set to (1,0,0) in this case.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of tangents, converted to `glm::vec3`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec3> tangents_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the bitangents for the mesh at the given index.
		 *	If the mesh has no bitangents, a vector filled with values is
		 *	returned regardless. All the values will be set to (0,1,0) in this case.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of bitangents, converted to `glm::vec3`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec3> bitangents_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the colors of a specific color set for the mesh at the given index.
		 *	If the mesh has no colors for the given set index, a vector filled with values is
		 *	returned regardless. All the values will be set to (1,0,1,1) in this case (magenta).
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@param		aSet			Index to a specific set of colors
		 *	@return		Vector of colors, converted to `glm::vec4`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec4> colors_for_mesh(mesh_index_t aMeshIndex, int aSet = 0) const;

		/** Gets all the bone weights for the mesh at the given index.
		 *	If the mesh has no bone weights, a vector filled with values is
		 *	returned regardless. All the values will be set to (1,0,0,0) in this case.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of bone weights, converted to `glm::vec4`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::vec4> bone_weights_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the bone indices for the mesh at the given index.
		 *	If the mesh has no bone indices, a vector filled with values is
		 *	returned regardless. All the values will be set to (0,0,0,0) in this case.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of bone indices, converted to `glm::uvec4`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		std::vector<glm::uvec4> bone_indices_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets the number of uv-components of a specific UV-set for the mesh at the given index
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@param		aSet			Index to a specific set of texture coordinates
		 *	@return		Number of uv components the given set has. This can, e.g., be used to 
		 *				determine how to retrieve the texture coordinates: as vec2 or as vec3, 
		 *				like follows: `texture_coordinates_for_mesh<vec2>(0)` or `texture_coordinates_for_mesh<vec3>(0)`, respectively.
		 */
		int num_uv_components_for_mesh(mesh_index_t aMeshIndex, int aSet = 0) const;

		/** Gets all the texture coordinates of a UV-set for the mesh at the given index.
		 *	If the mesh has no colors for the given set index, a vector filled with values is
		 *	returned regardless. You'll have to specify the type of UV-coordinates which you
		 *	want to retrieve. Supported types are `glm::vec2` and `glm::vec3`.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@param		aSet			Index to a specific set of UV-coordinates
		 *	@return		Vector of UV-coordinates, converted to `T`
		 *				of length `number_of_vertices_for_mesh()`
		 */
		template <typename T> std::vector<T> texture_coordinates_for_mesh(mesh_index_t aMeshIndex, int aSet = 0) const
		{
			throw gvk::logic_error(fmt::format("unsupported type {}", typeid(T).name()));
		}

		/** Gets the number of indices for the mesh at the given index.
		 *	Please note: Theoretically it can happen that a mesh has faces with different 
		 *	numbers of vertices (e.g. triangles and quads). Use the `aiProcess_Triangulate`
		 *	import flag to get only triangles, or make sure to handle them properly.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Number of indices for the given mesh.
		 */
		int number_of_indices_for_mesh(mesh_index_t aMeshIndex) const;

		/** Gets all the indices for the mesh at the given index.
		 *	@param		aMeshIndex		The index corresponding to the mesh
		 *	@return		Vector of vertex positions, converted to type `T`
		 *				of length `number_of_indices_for_mesh()`.
		 *				In most cases, you'll want to pass `uint16_t` or `uint32_t` for `T`.
		 */
		template <typename T> 
		std::vector<T> indices_for_mesh(mesh_index_t aMeshIndex) const
		{ 
			const aiMesh* paiMesh = mScene->mMeshes[aMeshIndex];
			size_t indicesCount = number_of_indices_for_mesh(aMeshIndex);
			std::vector<T> result;
			result.reserve(indicesCount);
			for (unsigned int i = 0; i < paiMesh->mNumFaces; ++i) {
				// we're working with triangulated meshes only
				const aiFace& paiFace = paiMesh->mFaces[i];
				for (unsigned int f = 0; f < paiFace.mNumIndices; ++f) {
					result.emplace_back(static_cast<T>(paiFace.mIndices[f]));
				}
			}
			return result;
		}

		/** Returns the number of meshes. */
		mesh_index_t num_meshes() const { return mScene->mNumMeshes; }
		
		/** Return the indices of all meshes which the given predicate evaluates true for.
		 *	Function-signature: bool(size_t, const aiMesh*) where the first parameter is the 
		 *									mesh index and the second the pointer to the data
		 */
		template <typename F>
		std::vector<size_t> select_meshes(F aPredicate) const
		{
			std::vector<size_t> result;
			for (size_t i = 0; i < mScene->mNumMeshes; ++i) {
				const aiMesh* paiMesh = mScene->mMeshes[i];
				if (aPredicate(i, paiMesh)) {
					result.push_back(i);
				}
			}
			return result;
		}

		/** Return the indices of all meshes. It's effecively the same as calling
		 *	`select_meshes` with a predicate that always evaluates true.
		 */
		std::vector<size_t> select_all_meshes() const;

		std::vector<glm::vec3> positions_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;
		std::vector<glm::vec3> normals_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;
		std::vector<glm::vec3> tangents_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;
		std::vector<glm::vec3> bitangents_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;
		std::vector<glm::vec4> colors_for_meshes(std::vector<mesh_index_t> aMeshIndices, int aSet = 0) const;
		std::vector<glm::vec4> bone_weights_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;
		std::vector<glm::uvec4> bone_indices_for_meshes(std::vector<mesh_index_t> aMeshIndices) const;

		template <typename T>
		std::vector<T> texture_coordinates_for_meshes(std::vector<mesh_index_t> aMeshIndices, int aSet = 0) const
		{
			std::vector<T> result;
			for (auto meshIndex : aMeshIndices) {
				auto tmp = texture_coordinates_for_mesh<T>(meshIndex, aSet);
				std::move(std::begin(tmp), std::end(tmp), std::back_inserter(result));
			}
			return result;
		}

		template <typename T>
		std::vector<T> indices_for_meshes(std::vector<mesh_index_t> aMeshIndices) const
		{
			std::vector<T> result;
			for (auto meshIndex : aMeshIndices) {
				auto tmp = indices_for_mesh<T>(meshIndex);
				std::move(std::begin(tmp), std::end(tmp), std::back_inserter(result));
			}
			return result;
		}

		/** Returns all lightsources stored in the model file */
		std::vector<lightsource> lights() const;

		/** Returns all cameras stored in the model file */
		std::vector<gvk::camera> cameras() const;

		/** Load an animation clip's data */
		animation_clip_data load_animation_clip(unsigned int aAnimationIndex, double aStartTimeTicks, double aEndTimeTicks) const;



static glm::mat4 aiMat4_to_glmMat4(const aiMatrix4x4 &ai) {
	glm::mat4 g;
	g[0][0] = ai[0][0]; g[0][1] = ai[1][0]; g[0][2] = ai[2][0]; g[0][3] = ai[3][0];
	g[1][0] = ai[0][1]; g[1][1] = ai[1][1]; g[1][2] = ai[2][1]; g[1][3] = ai[3][1];
	g[2][0] = ai[0][2]; g[2][1] = ai[1][2]; g[2][2] = ai[2][2]; g[2][3] = ai[3][2];
	g[3][0] = ai[0][3]; g[3][1] = ai[1][3]; g[3][2] = ai[2][3]; g[3][3] = ai[3][3];
	return g;
}

static glm::vec3 aiVec3_to_glmVec3(const aiVector3D &ai) {
	return glm::vec3(ai.x, ai.y, ai.z);
}

template<typename T>
static double getInterpolationFrames(T *keys, size_t numKeys, double ticks, double duration, size_t &priorFrame, size_t &nextFrame) {
	// find next key frame (after current time)
	assert(numKeys > 0);
	for (size_t i = 1; i < numKeys; i++) {
		if (ticks <= keys[i].mTime) {
			priorFrame = i - 1;
			nextFrame  = i;
			return (ticks - keys[priorFrame].mTime) / (keys[nextFrame].mTime - keys[priorFrame].mTime);
		}
	}
	// interpolate between last and first frame ... does that ever happen?
	priorFrame = numKeys - 1;
	nextFrame  = 0;
	return (ticks - keys[priorFrame].mTime) / (keys[nextFrame].mTime + duration - keys[priorFrame].mTime);
}

void interpolateScaling(aiNodeAnim * nodeAnim, glm::mat4 & result, const animation_clip_data& aAnimationClip, double ticks, double duration) {
	aiVector3D scale;
	if (nodeAnim->mNumScalingKeys == 1) {
		scale = nodeAnim->mScalingKeys[0].mValue;
	} else {
		size_t prior, next;
		float factor = (float)getInterpolationFrames(nodeAnim->mScalingKeys, nodeAnim->mNumScalingKeys, ticks, duration, prior, next);
		aiVector3D &a = nodeAnim->mScalingKeys[prior].mValue;
		aiVector3D &b = nodeAnim->mScalingKeys[next] .mValue;
		scale = a + factor * (b - a);
	}
	result = glm::scale(glm::mat4(1.f), aiVec3_to_glmVec3(scale));
}

void interpolateTranslation(aiNodeAnim * nodeAnim, glm::mat4 & result, const animation_clip_data& aAnimationClip, double ticks, double duration) {
	aiVector3D trans;
	if (nodeAnim->mNumPositionKeys == 1) {
		trans = nodeAnim->mPositionKeys[0].mValue;
	} else {
		size_t prior, next;
		float factor = (float)getInterpolationFrames(nodeAnim->mPositionKeys, nodeAnim->mNumPositionKeys, ticks, duration, prior, next);
		aiVector3D &a = nodeAnim->mPositionKeys[prior].mValue;
		aiVector3D &b = nodeAnim->mPositionKeys[next] .mValue;
		trans = a + factor * (b - a);
	}
	result = glm::translate(glm::mat4(1.f), aiVec3_to_glmVec3(trans));
}

void interpolateRotation(aiNodeAnim * nodeAnim, glm::mat4 & result, const animation_clip_data& aAnimationClip, double ticks, double duration) {
	aiQuaternion q;
	if (nodeAnim->mNumRotationKeys == 1) {
		q = nodeAnim->mRotationKeys[0].mValue;
	} else {
		size_t prior, next;
		float factor = (float)getInterpolationFrames(nodeAnim->mRotationKeys, nodeAnim->mNumRotationKeys, ticks, duration, prior, next);
		aiQuaternion &a = nodeAnim->mRotationKeys[prior].mValue;
		aiQuaternion &b = nodeAnim->mRotationKeys[next] .mValue;
		aiQuaternion::Interpolate(q, a, b, factor);
		q = q.Normalize();
	}
	result = aiMat4_to_glmMat4(aiMatrix4x4(q.GetMatrix()));
}


	// Find animation for a given node
	const aiNodeAnim* findNodeAnim(const aiAnimation* animation, const std::string nodeName)
	{
		for (uint32_t i = 0; i < animation->mNumChannels; i++)
		{
			const aiNodeAnim* nodeAnim = animation->mChannels[i];
			if (std::string(nodeAnim->mNodeName.data) == nodeName)
			{
				return nodeAnim;
			}
		}
		return nullptr;
	}

	// Load bone information from ASSIMP mesh
	std::optional<uint32_t> getBoneIndex(const aiMesh* pMesh, std::string aName)
	{
		for (uint32_t i = 0; i < pMesh->mNumBones; i++) {
			if (std::string(pMesh->mBones[i]->mName.data) == aName) {
				return i;
			}
		}
		return {};
	}
		
	// Get node hierarchy for current animation time
	void readNodeHierarchy(glm::mat4* aBoneMatricesStorage, mesh_index_t aMeshIndex, const animation_clip_data& aAnimationClip, float aAnimationTime, aiNode* node, const glm::mat4& parentTransform, animated_meshes& aMeshesToAnimate)
	{
		for (auto& meshAni : aMeshesToAnimate.mMeshIndicesAndTargetStorage) {
			auto meshIndex = std::get<mesh_index_t>(meshAni);
			auto storagePtr = std::get<glm::mat4*>(meshAni);

			
			// find the nodeAnim for the current node
			auto animIter = aMeshesToAnimate.m_nodeToNodeAnimMap.find(node);
			aiNodeAnim *nodeAnim = animIter != aMeshesToAnimate.m_nodeToNodeAnimMap.end() ? animIter->second : nullptr;

			glm::mat4 nodeTransform = aiMat4_to_glmMat4(node->mTransformation);
			if (nodeAnim) {
				// interpolate
				glm::mat4 scaling,rotation,translation;
				interpolateScaling(nodeAnim, scaling, aAnimationClip, aAnimationTime, aAnimationClip.mEndTicks - aAnimationClip.mStartTicks);
				interpolateRotation(nodeAnim, rotation, aAnimationClip, aAnimationTime, aAnimationClip.mEndTicks - aAnimationClip.mStartTicks);
				interpolateTranslation(nodeAnim, translation, aAnimationClip, aAnimationTime, aAnimationClip.mEndTicks - aAnimationClip.mStartTicks);
				nodeTransform = translation * rotation * scaling;
			}


			glm::mat4 globalTransform = parentTransform * nodeTransform;

			// update the final transform for this bone (if it is a bone)
			std::string	nodeName(node->mName.C_Str());
			auto boneIter = aMeshesToAnimate.m_boneNameIdMap.find(nodeName);
			if (boneIter != aMeshesToAnimate.m_boneNameIdMap.end()) {
				uint32_t boneId = boneIter->second;
				auto m_globalInverseTransform = glm::inverse(aiMat4_to_glmMat4(mScene->mRootNode->mTransformation));
				aMeshesToAnimate.m_boneFinalTransform[boneId] = m_globalInverseTransform * globalTransform * aMeshesToAnimate.m_boneOffsetMatrix[boneId];
			}

			// process children
			for (size_t i = 0; i < node->mNumChildren; i++)
				readNodeHierarchy(aBoneMatricesStorage, aMeshIndex, aAnimationClip, aAnimationTime, node->mChildren[i], globalTransform, aMeshesToAnimate);
		
			//auto& target = aBoneMatricesStorage[boneIndex.value()];
			//target[0][0] = finalTransform.a1;
			//target[0][1] = finalTransform.b1;
			//target[0][2] = finalTransform.c1;
			//target[0][3] = finalTransform.d1;
			//target[1][0] = finalTransform.a2;
			//target[1][1] = finalTransform.b2;
			//target[1][2] = finalTransform.c2;
			//target[1][3] = finalTransform.d2;
			//target[2][0] = finalTransform.a3;
			//target[2][1] = finalTransform.b3;
			//target[2][2] = finalTransform.c3;
			//target[2][3] = finalTransform.d3;
			//target[3][0] = finalTransform.a4;
			//target[3][1] = finalTransform.b4;
			//target[3][2] = finalTransform.c4;
			//target[3][3] = finalTransform.d4;
		}
	}

void initNodeAnimMap(std::map<aiNode *, aiNodeAnim *>& m_nodeToNodeAnimMap, aiAnimation *m_animation, aiNode * node) {
	if (m_nodeToNodeAnimMap.find(node) == m_nodeToNodeAnimMap.end()) {
		for (size_t i = 0; i < m_animation->mNumChannels; i++) {
			if (m_animation->mChannels[i]->mNodeName == node->mName) {
				m_nodeToNodeAnimMap[node] = m_animation->mChannels[i];
				break;
			}
		}
	}
	for (size_t i = 0; i < node->mNumChildren; i++)
		initNodeAnimMap(m_nodeToNodeAnimMap, m_animation, node->mChildren[i]);
}
		
		animated_meshes declare_to_animate_all_meshes_into_strided_consecutive_storage(glm::mat4* aBeginningOfTargetStorage, size_t aStride, std::optional<size_t> aMaxNumBoneMatrices = {})
		{
			if (!aMaxNumBoneMatrices.has_value()) {
				aMaxNumBoneMatrices = aStride;
			}
		
			auto numMeshes = num_meshes();
			animated_meshes result;
			for (decltype(numMeshes) i = 0; i < numMeshes; ++i) {
				result.mMeshIndicesAndTargetStorage.emplace_back(i, aBeginningOfTargetStorage + i * aStride);
			}
			result.mMaxNumBoneMatrices = aMaxNumBoneMatrices.value();

	
			result.m_boneFinalTransform.resize(aMaxNumBoneMatrices.value());
			initNodeAnimMap(result.m_nodeToNodeAnimMap, mScene->mAnimations[0], mScene->mRootNode);


			result.m_boneNameIdMap.clear();
			result.m_boneOffsetMatrix.clear();
			for (size_t iMesh = 0; iMesh < mScene->mNumMeshes; iMesh++) {
				aiMesh *aimesh = mScene->mMeshes[iMesh];
				for (size_t iBone = 0; iBone < aimesh->mNumBones; iBone++) {
					aiBone *aibone = aimesh->mBones[iBone];
					std::string name(aibone->mName.C_Str());
					uint32_t boneId;
					if (result.m_boneNameIdMap.find(name) == result.m_boneNameIdMap.end()) {
						// new bone, add it
						boneId = (uint32_t)result.m_boneOffsetMatrix.size();
						result.m_boneNameIdMap[name] = boneId;
						result.m_boneOffsetMatrix.push_back(aiMat4_to_glmMat4(aibone->mOffsetMatrix));
					} else {
						// already there
						boneId = result.m_boneNameIdMap[name];
	//#if _DEBUG
	//					if (m_boneOffsetMatrix[boneId] != aiMat4_to_glmMat4(aibone->mOffsetMatrix))
	//						FATAL_ERROR("Bone offset matrix mismatch on bone " << name << " in mesh " << iMesh);
	//#endif
					}

				}
			}

		
			return result;
		}
		
		animated_meshes declare_to_animate_all_meshes_into_tightly_packed_consecutive_storage(glm::mat4* aBeginningOfTargetStorage, size_t aMaxNumBoneMatrices)
		{
			return declare_to_animate_all_meshes_into_strided_consecutive_storage(aBeginningOfTargetStorage, aMaxNumBoneMatrices, aMaxNumBoneMatrices);
		}
		
		void update_bone_matrices(animated_meshes& aMeshesToAnimate, const animation_clip_data& aAnimationClip, double aTime)
		{
			for (auto& aniMesh : aMeshesToAnimate.mMeshIndicesAndTargetStorage) {
				glm::mat4 identity(1.0f);
				auto meshIndex = std::get<mesh_index_t>(aniMesh);
				auto* targetStorage = std::get<glm::mat4*>(aniMesh);
				readNodeHierarchy(targetStorage, meshIndex, aAnimationClip, aTime, mScene->mRootNode, identity, aMeshesToAnimate);
			}
		}

	private:
		void initialize_materials();
		std::optional<glm::mat4> transformation_matrix_traverser(const unsigned int aMeshIndexToFind, const aiNode* aNode, const aiMatrix4x4& aM) const;
		std::optional<glm::mat4> transformation_matrix_traverser_for_light(const aiLight* aLight, const aiNode* Node, const aiMatrix4x4& aM) const;
		std::optional<glm::mat4> transformation_matrix_traverser_for_camera(const aiCamera* aCamera, const aiNode* aNode, const aiMatrix4x4& aM) const;

		std::unique_ptr<Assimp::Importer> mImporter;
		std::string mModelPath;
		const aiScene* mScene;
		std::vector<std::optional<material_config>> mMaterialConfigPerMesh;
	};

	using model = avk::owning_resource<model_t>;


	template <>
	inline std::vector<glm::vec2> model_t::texture_coordinates_for_mesh<glm::vec2>(mesh_index_t _MeshIndex, int _Set) const
	{
		const aiMesh* paiMesh = mScene->mMeshes[_MeshIndex];
		auto n = paiMesh->mNumVertices;
		std::vector<glm::vec2> result;
		result.reserve(n);
		assert(_Set >= 0 && _Set < AI_MAX_NUMBER_OF_TEXTURECOORDS);
		if (nullptr == paiMesh->mTextureCoords[_Set]) {
			LOG_WARNING(fmt::format("The mesh at index {} does not contain a texture coordinates at index {}. Will return (0,0) for each vertex.", _MeshIndex, _Set));
			result.emplace_back(0.f, 0.f);
		}
		else {
			const auto nuv = num_uv_components_for_mesh(_MeshIndex, _Set);
			switch (nuv) {
			case 1:
				for (decltype(n) i = 0; i < n; ++i) {
					result.emplace_back(paiMesh->mTextureCoords[_Set][i][0], 0.f);
				}
				break;
			case 2:
			case 3:
				for (decltype(n) i = 0; i < n; ++i) {
					result.emplace_back(paiMesh->mTextureCoords[_Set][i][0], paiMesh->mTextureCoords[_Set][i][1]);
				}
				break;
			default:
				throw gvk::logic_error(fmt::format("Can't handle a number of {} uv components for mesh at index {}, set {}.", nuv, _MeshIndex, _Set));
			}
		}
		return result;
	}

	template <>
	inline std::vector<glm::vec3> model_t::texture_coordinates_for_mesh<glm::vec3>(mesh_index_t _MeshIndex, int _Set) const
	{
		const aiMesh* paiMesh = mScene->mMeshes[_MeshIndex];
		auto n = paiMesh->mNumVertices;
		std::vector<glm::vec3> result;
		result.reserve(n);
		assert(_Set >= 0 && _Set < AI_MAX_NUMBER_OF_TEXTURECOORDS);
		if (nullptr == paiMesh->mTextureCoords[_Set]) {
			LOG_WARNING(fmt::format("The mesh at index {} does not contain a texture coordinates at index {}. Will return (0,0,0) for each vertex.", _MeshIndex, _Set));
			result.emplace_back(0.f, 0.f, 0.f);
		}
		else {
			const auto nuv = num_uv_components_for_mesh(_MeshIndex, _Set);
			switch (nuv) {
			case 1:
				for (decltype(n) i = 0; i < n; ++i) {
					result.emplace_back(paiMesh->mTextureCoords[_Set][i][0], 0.f, 0.f);
				}
				break;
			case 2:
				for (decltype(n) i = 0; i < n; ++i) {
					result.emplace_back(paiMesh->mTextureCoords[_Set][i][0], paiMesh->mTextureCoords[_Set][i][1], 0.f);
				}
				break;
			case 3:
				for (decltype(n) i = 0; i < n; ++i) {
					result.emplace_back(paiMesh->mTextureCoords[_Set][i][0], paiMesh->mTextureCoords[_Set][i][1], paiMesh->mTextureCoords[_Set][i][2]);
				}
				break;
			default:
				throw gvk::logic_error(fmt::format("Can't handle a number of {} uv components for mesh at index {}, set {}.", nuv, _MeshIndex, _Set));
			}
		}
		return result;
	}

	/** Helper function used by `cgb::append_indices_and_vertex_data` */
	template <typename Vert>
	size_t get_vertex_count(const Vert& _First)
	{
		return _First.size();
	}

	/** Helper function used by `cgb::append_indices_and_vertex_data` */
	template <typename Vert, typename... Verts>
	size_t get_vertex_count(const Vert& _First, const Verts&... _Rest)
	{
#if defined(_DEBUG) 
		// Check whether all of the vertex data has the same length!
		auto countOfNext = get_vertex_count(_Rest...);
		if (countOfNext != _First.size()) {
			throw gvk::logic_error(fmt::format("The vertex data passed are not all of the same length, namely {} vs. {}.", countOfNext, _First.size()));
		}
#endif
		return _First.size();
	}

	/** Inserts the elements from the collection `_ToInsert` into the collection `_Destination`. */
	template <typename V>
	void insert_into(V& _Destination, const V& _ToInsert)
	{
		_Destination.insert(std::end(_Destination), std::begin(_ToInsert), std::end(_ToInsert));
	}

	/** Inserts the elements from the collection `_ToInsert` into the collection `_Destination` and adds `_ToAdd` to them. */
	template <typename V, typename A>
	void insert_into_and_add(V& _Destination, const V& _ToInsert, A _ToAdd)
	{
		_Destination.reserve(_Destination.size() + _ToInsert.size());
		auto addValType = static_cast<typename V::value_type>(_ToAdd);
		for (auto& e : _ToInsert) {
			_Destination.push_back(e + addValType);
		}
	}

	/** Utility function to concatenate lists of vertex data and according lists of index data.
	 *	The vertex data is concatenated unmodified, and an arbitrary number of vertex data vectors is supported.
	 *	The index data, however, will be modified during concatenation to account for the vertices which come before.
	 *
	 *	Example: 
	 *	If there are already 100 vertices in the vertex data vectors, adding the indices 0, 2, 1 will result in
	 *	actually the values 100+0, 100+2, 100+1, i.e. 100, 102, 101, being added to the vector of existing indices.
	 *
	 *	Usage:
	 *	This method takes `std::tuple`s as parameters to assign source collections to destination collections.
	 *	The destinations are referring to collections, while the sources must be lambdas providing the data.
	 *		Example: `std::vector<glm::vec3> positions;` for the first parameter and `[&]() { return someModel->positions_for_meshes({ 0 }); }` for the second parameter.
	 *	Please note that the first parameter of these tuples is captured by reference, which requires 
	 *	`std::forward_as_tuple` to be used. For better readability, `cgb::additional_index_data` and
	 *	`cgb::additional_vertex_data` can be used instead, which are actually just the same as `std::forward_as_tuple`.
	 *
	 *
	 */
	template <typename... Vert, typename... Getter, typename Ind, typename IndGetter>
	void append_indices_and_vertex_data(std::tuple<Ind&, IndGetter> _IndDstAndGetter, std::tuple<Vert&, Getter>... _VertDstAndGetterPairs)
	{
		// Count vertices BEFORE appending!
		auto vertexCount = get_vertex_count(std::get<0>(_VertDstAndGetterPairs)...);
		// Append all the vertex data:
		(insert_into(/* Existing vector: */ std::get<0>(_VertDstAndGetterPairs), /* Getter: */ std::move(std::get<1>(_VertDstAndGetterPairs)())), ...);
		// Append the index data:
		insert_into_and_add(std::get<0>(_IndDstAndGetter), std::get<1>(_IndDstAndGetter)(), vertexCount);
		//insert_into_and_add(_A, _B(), vertexCount);
	}

	/** This is actually just an alias to `std::forward_as_tuple`. It does not add any functionality,
	 *	but it should help to express the intent better. Use it with `cgb::append_vertex_data_and_indices`!
	 */
	template <class... _Types>
	_NODISCARD constexpr std::tuple<_Types&&...> additional_vertex_data(_Types&&... _Args) noexcept {
		return std::forward_as_tuple(std::forward<_Types>(_Args)...);
	}

	/** This is actually just an alias to `std::forward_as_tuple`. It does not add any functionality,
	 *	but it should help to express the intent better. Use it with `cgb::append_vertex_data_and_indices`!
	 */
	template <class... _Types>
	_NODISCARD constexpr std::tuple<_Types&&...> additional_index_data(_Types&&... _Args) noexcept {
		return std::forward_as_tuple(std::forward<_Types>(_Args)...);
	}

	///** This is a convenience function and is actually just an alias to `std::forward_as_tuple`. It does not add any functionality,
	// *	but it should help to express the intent better. 
	// */
	//template <typename M>
	//_NODISCARD constexpr std::tuple<std::reference_wrapper<model_t>, std::vector<size_t>> make_tuple_model_and_indices(const M& _Model, std::vector<mesh_index_t> _Indices) noexcept {
	//	return std::forward_as_tuple<std::reference_wrapper<model_t>, std::vector<size_t>>(std::ref(_Model), std::move(_Indices));
	//}


}
