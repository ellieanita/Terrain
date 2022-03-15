// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "S_kijai_water_simple"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][Header(Water Normals)][NoScaleOffset][Normal]_NormalMap_01("NormalMap_01", 2D) = "bump" {}
		[Header(Normal directions in degrees)]_Normal_DirectionAngle_01("Normal_DirectionAngle_01", Range( 0 , 360)) = 0
		[NoScaleOffset][Normal]_NormalMap_02("NormalMap_02", 2D) = "bump" {}
		_Normal_DirectionAngle_02("Normal_DirectionAngle_02", Range( 45 , 360)) = 45
		_NormalMapTiling("NormalMapTiling", Float) = 10
		_NormalSpeed("NormalSpeed", Range( 0 , 1)) = 0.1
		_NormalStrength("NormalStrength", Range( 0 , 1)) = 1
		_MipMapBias("MipMapBias", Range( 0 , 5)) = 1
		_HighLightDensity("HighLightDensity", Range( 0 , 1)) = 1
		[Header(PBR Values)]_Metallic("Metallic", Range( 0 , 1)) = 0
		_Smoothness("Smoothness", Range( 0 , 1)) = 0.81
		_Opacity("Opacity", Range( 0 , 1)) = 1
		_Color_Shallow("Color_Shallow", Color) = (0.3221193,0.4825942,0.8396226,0)
		_Color_Deep("Color_Deep", Color) = (0.3221193,0.4825942,0.8396226,0)
		[Header(Depth)]_FoamScale("FoamScale", Float) = 1
		_EdgePower("EdgePower", Range( 0 , 1)) = 0.576821
		_RefractionStrenght("RefractionStrenght", Float) = 0
		[Toggle(_REFRACTION_ON)] _Refraction("Refraction", Float) = 1
		[Toggle(_FOAMTOGGLE_ON)] _FoamToggle("FoamToggle", Float) = 1
		_Depth("Depth", Range( 0 , 1)) = 1
		[Header(Displacement)]_DisplacementFrequency("DisplacementFrequency", Range( 0 , 1)) = 1
		_DisplacementAmplitude("DisplacementAmplitude", Range( 0 , 0.5)) = 0.1
		[Toggle(_DISPLACEMENTTOGGLE_ON)] _DisplacementToggle("DisplacementToggle", Float) = 1
		[Header(Fade distant water)]_FadeDistance("FadeDistance", Float) = 1
		_DistanceFadeHardness("DistanceFadeHardness", Range( 0 , 1)) = 1
		[KeywordEnum(camera,water_center)] _FadeFrom("FadeFrom", Float) = 0
		[ASEEnd][Toggle(_DISTANCEFADETOGGLE_ON)] _DistanceFadeToggle("DistanceFadeToggle", Float) = 1

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }
		Cull Back
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 3.0

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma multi_compile _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS _ADDITIONAL_OFF
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_SCREEN_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _REFRACTION_ON
			#pragma shader_feature_local _FOAMTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _NormalMap_01;
			sampler2D _NormalMap_02;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				float3 objectToViewPos = TransformWorldToView(TransformObjectToWorld(v.vertex.xyz));
				float eyeDepth = -objectToViewPos.z;
				o.ase_texcoord7.z = eyeDepth;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				half mulTime14 = _TimeParameters.x * _NormalSpeed;
				half temp_output_30_0 = radians( _Normal_DirectionAngle_01 );
				half2 appendResult26 = (half2(sin( temp_output_30_0 ) , cos( temp_output_30_0 )));
				half2 temp_cast_0 = (_NormalMapTiling).xx;
				half2 texCoord20 = IN.ase_texcoord7.xy * temp_cast_0 + float2( 0,0 );
				half2 panner11 = ( mulTime14 * appendResult26 + texCoord20);
				half temp_output_181_0 = ( 1.0 - _NormalStrength );
				half3 lerpResult179 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_01, float4( panner11, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0139 = lerpResult179;
				half temp_output_34_0 = radians( _Normal_DirectionAngle_02 );
				half2 appendResult32 = (half2(sin( temp_output_34_0 ) , cos( temp_output_34_0 )));
				half2 panner12 = ( mulTime14 * appendResult32 + texCoord20);
				half3 lerpResult180 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_02, float4( panner12, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0240 = lerpResult180;
				half3 BlendedPannedNormalMaps37 = BlendNormal( xyzNormalMap_0139 , xyzNormalMap_0240 );
				float eyeDepth = IN.ase_texcoord7.z;
				half eyeDepth28_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				half2 temp_output_20_0_g1 = ( (BlendedPannedNormalMaps37).xy * ( _RefractionStrenght / max( eyeDepth , 0.1 ) ) * saturate( ( eyeDepth28_g1 - eyeDepth ) ) );
				half eyeDepth2_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( half4( temp_output_20_0_g1, 0.0 , 0.0 ) + ase_screenPosNorm ).xy ),_ZBufferParams);
				half2 temp_output_32_0_g1 = (( half4( ( temp_output_20_0_g1 * saturate( ( eyeDepth2_g1 - eyeDepth ) ) ), 0.0 , 0.0 ) + ase_screenPosNorm )).xy;
				half2 temp_output_74_38 = temp_output_32_0_g1;
				#ifdef _REFRACTION_ON
				half4 staticSwitch100 = half4( temp_output_74_38, 0.0 , 0.0 );
				#else
				half4 staticSwitch100 = ase_screenPosNorm;
				#endif
				half eyeDepth99 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( staticSwitch100.xy ),_ZBufferParams);
				half temp_output_82_0 = ( eyeDepth99 - eyeDepth );
				half depth69 = ( temp_output_82_0 * _Depth );
				half temp_output_105_0 = saturate( depth69 );
				half4 lerpResult104 = lerp( _Color_Shallow , _Color_Deep , temp_output_105_0);
				#ifdef _FOAMTOGGLE_ON
				half4 staticSwitch106 = lerpResult104;
				#else
				half4 staticSwitch106 = _Color_Deep;
				#endif
				half4 fetchOpaqueVal75 = float4( SHADERGRAPH_SAMPLE_SCENE_COLOR( temp_output_74_38 ), 1.0 );
				half4 lerpResult172 = lerp( _Color_Deep , fetchOpaqueVal75 , temp_output_105_0);
				#ifdef _REFRACTION_ON
				half4 staticSwitch77 = ( lerpResult104 + lerpResult172 );
				#else
				half4 staticSwitch77 = staticSwitch106;
				#endif
				
				half3 normalizeResult45 = normalize( xyzNormalMap_0139 );
				half3 normalizeResult46 = normalize( xyzNormalMap_0240 );
				half dotResult53 = dot( normalizeResult45 , normalizeResult46 );
				half dotResult47 = dot( xyzNormalMap_0139.x , xyzNormalMap_0240.x );
				half clampResult48 = clamp( dotResult47 , 0.0 , 1.0 );
				half clampResult52 = clamp( ( pow( clampResult48 , ( 1.0 - _HighLightDensity ) ) * 3.3 ) , 0.0 , 1.0 );
				half NormalHighlights59 = ( ( pow( abs( ( ( dotResult53 - 0.005 ) * clampResult52 ) ) , 3800.0 ) * 50000000.0 ) * 5000.0 );
				#ifdef _FOAMTOGGLE_ON
				half staticSwitch97 = ( ( 1.0 - saturate( ( temp_output_82_0 * 12.0 ) ) ) + ( ( 1.0 - saturate( ( temp_output_82_0 + _FoamScale ) ) ) * _EdgePower ) );
				#else
				half staticSwitch97 = 0.0;
				#endif
				half3 temp_cast_5 = (( NormalHighlights59 + staticSwitch97 )).xxx;
				
				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				float3 Albedo = staticSwitch77.rgb;
				float3 Normal = BlendedPannedNormalMaps37;
				float3 Emission = temp_cast_5;
				float3 Specular = 0.5;
				float Metallic = _Metallic;
				float Smoothness = _Smoothness;
				float Occlusion = 1;
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.clipPos);
				inputData.shadowMask = SAMPLE_SHADOWMASK(IN.lightmapUVOrVertexSH.xy);

				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, WorldNormal ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif

				return 0;
			}
			ENDHLSL
		}
		
		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _REFRACTION_ON
			#pragma shader_feature_local _FOAMTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _NormalMap_01;
			sampler2D _NormalMap_02;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				float3 objectToViewPos = TransformWorldToView(TransformObjectToWorld(v.vertex.xyz));
				float eyeDepth = -objectToViewPos.z;
				o.ase_texcoord3.z = eyeDepth;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				half mulTime14 = _TimeParameters.x * _NormalSpeed;
				half temp_output_30_0 = radians( _Normal_DirectionAngle_01 );
				half2 appendResult26 = (half2(sin( temp_output_30_0 ) , cos( temp_output_30_0 )));
				half2 temp_cast_0 = (_NormalMapTiling).xx;
				half2 texCoord20 = IN.ase_texcoord3.xy * temp_cast_0 + float2( 0,0 );
				half2 panner11 = ( mulTime14 * appendResult26 + texCoord20);
				half temp_output_181_0 = ( 1.0 - _NormalStrength );
				half3 lerpResult179 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_01, float4( panner11, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0139 = lerpResult179;
				half temp_output_34_0 = radians( _Normal_DirectionAngle_02 );
				half2 appendResult32 = (half2(sin( temp_output_34_0 ) , cos( temp_output_34_0 )));
				half2 panner12 = ( mulTime14 * appendResult32 + texCoord20);
				half3 lerpResult180 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_02, float4( panner12, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0240 = lerpResult180;
				half3 BlendedPannedNormalMaps37 = BlendNormal( xyzNormalMap_0139 , xyzNormalMap_0240 );
				float eyeDepth = IN.ase_texcoord3.z;
				half eyeDepth28_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				half2 temp_output_20_0_g1 = ( (BlendedPannedNormalMaps37).xy * ( _RefractionStrenght / max( eyeDepth , 0.1 ) ) * saturate( ( eyeDepth28_g1 - eyeDepth ) ) );
				half eyeDepth2_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( half4( temp_output_20_0_g1, 0.0 , 0.0 ) + ase_screenPosNorm ).xy ),_ZBufferParams);
				half2 temp_output_32_0_g1 = (( half4( ( temp_output_20_0_g1 * saturate( ( eyeDepth2_g1 - eyeDepth ) ) ), 0.0 , 0.0 ) + ase_screenPosNorm )).xy;
				half2 temp_output_74_38 = temp_output_32_0_g1;
				#ifdef _REFRACTION_ON
				half4 staticSwitch100 = half4( temp_output_74_38, 0.0 , 0.0 );
				#else
				half4 staticSwitch100 = ase_screenPosNorm;
				#endif
				half eyeDepth99 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( staticSwitch100.xy ),_ZBufferParams);
				half temp_output_82_0 = ( eyeDepth99 - eyeDepth );
				half depth69 = ( temp_output_82_0 * _Depth );
				half temp_output_105_0 = saturate( depth69 );
				half4 lerpResult104 = lerp( _Color_Shallow , _Color_Deep , temp_output_105_0);
				#ifdef _FOAMTOGGLE_ON
				half4 staticSwitch106 = lerpResult104;
				#else
				half4 staticSwitch106 = _Color_Deep;
				#endif
				half4 fetchOpaqueVal75 = float4( SHADERGRAPH_SAMPLE_SCENE_COLOR( temp_output_74_38 ), 1.0 );
				half4 lerpResult172 = lerp( _Color_Deep , fetchOpaqueVal75 , temp_output_105_0);
				#ifdef _REFRACTION_ON
				half4 staticSwitch77 = ( lerpResult104 + lerpResult172 );
				#else
				half4 staticSwitch77 = staticSwitch106;
				#endif
				
				half3 normalizeResult45 = normalize( xyzNormalMap_0139 );
				half3 normalizeResult46 = normalize( xyzNormalMap_0240 );
				half dotResult53 = dot( normalizeResult45 , normalizeResult46 );
				half dotResult47 = dot( xyzNormalMap_0139.x , xyzNormalMap_0240.x );
				half clampResult48 = clamp( dotResult47 , 0.0 , 1.0 );
				half clampResult52 = clamp( ( pow( clampResult48 , ( 1.0 - _HighLightDensity ) ) * 3.3 ) , 0.0 , 1.0 );
				half NormalHighlights59 = ( ( pow( abs( ( ( dotResult53 - 0.005 ) * clampResult52 ) ) , 3800.0 ) * 50000000.0 ) * 5000.0 );
				#ifdef _FOAMTOGGLE_ON
				half staticSwitch97 = ( ( 1.0 - saturate( ( temp_output_82_0 * 12.0 ) ) ) + ( ( 1.0 - saturate( ( temp_output_82_0 + _FoamScale ) ) ) * _EdgePower ) );
				#else
				half staticSwitch97 = 0.0;
				#endif
				half3 temp_cast_5 = (( NormalHighlights59 + staticSwitch97 )).xxx;
				
				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				
				float3 Albedo = staticSwitch77.rgb;
				float3 Emission = temp_cast_5;
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _REFRACTION_ON
			#pragma shader_feature_local _FOAMTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _NormalMap_01;
			sampler2D _NormalMap_02;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				float3 objectToViewPos = TransformWorldToView(TransformObjectToWorld(v.vertex.xyz));
				float eyeDepth = -objectToViewPos.z;
				o.ase_texcoord3.z = eyeDepth;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				half mulTime14 = _TimeParameters.x * _NormalSpeed;
				half temp_output_30_0 = radians( _Normal_DirectionAngle_01 );
				half2 appendResult26 = (half2(sin( temp_output_30_0 ) , cos( temp_output_30_0 )));
				half2 temp_cast_0 = (_NormalMapTiling).xx;
				half2 texCoord20 = IN.ase_texcoord3.xy * temp_cast_0 + float2( 0,0 );
				half2 panner11 = ( mulTime14 * appendResult26 + texCoord20);
				half temp_output_181_0 = ( 1.0 - _NormalStrength );
				half3 lerpResult179 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_01, float4( panner11, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0139 = lerpResult179;
				half temp_output_34_0 = radians( _Normal_DirectionAngle_02 );
				half2 appendResult32 = (half2(sin( temp_output_34_0 ) , cos( temp_output_34_0 )));
				half2 panner12 = ( mulTime14 * appendResult32 + texCoord20);
				half3 lerpResult180 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_02, float4( panner12, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0240 = lerpResult180;
				half3 BlendedPannedNormalMaps37 = BlendNormal( xyzNormalMap_0139 , xyzNormalMap_0240 );
				float eyeDepth = IN.ase_texcoord3.z;
				half eyeDepth28_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				half2 temp_output_20_0_g1 = ( (BlendedPannedNormalMaps37).xy * ( _RefractionStrenght / max( eyeDepth , 0.1 ) ) * saturate( ( eyeDepth28_g1 - eyeDepth ) ) );
				half eyeDepth2_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( half4( temp_output_20_0_g1, 0.0 , 0.0 ) + ase_screenPosNorm ).xy ),_ZBufferParams);
				half2 temp_output_32_0_g1 = (( half4( ( temp_output_20_0_g1 * saturate( ( eyeDepth2_g1 - eyeDepth ) ) ), 0.0 , 0.0 ) + ase_screenPosNorm )).xy;
				half2 temp_output_74_38 = temp_output_32_0_g1;
				#ifdef _REFRACTION_ON
				half4 staticSwitch100 = half4( temp_output_74_38, 0.0 , 0.0 );
				#else
				half4 staticSwitch100 = ase_screenPosNorm;
				#endif
				half eyeDepth99 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( staticSwitch100.xy ),_ZBufferParams);
				half temp_output_82_0 = ( eyeDepth99 - eyeDepth );
				half depth69 = ( temp_output_82_0 * _Depth );
				half temp_output_105_0 = saturate( depth69 );
				half4 lerpResult104 = lerp( _Color_Shallow , _Color_Deep , temp_output_105_0);
				#ifdef _FOAMTOGGLE_ON
				half4 staticSwitch106 = lerpResult104;
				#else
				half4 staticSwitch106 = _Color_Deep;
				#endif
				half4 fetchOpaqueVal75 = float4( SHADERGRAPH_SAMPLE_SCENE_COLOR( temp_output_74_38 ), 1.0 );
				half4 lerpResult172 = lerp( _Color_Deep , fetchOpaqueVal75 , temp_output_105_0);
				#ifdef _REFRACTION_ON
				half4 staticSwitch77 = ( lerpResult104 + lerpResult172 );
				#else
				half4 staticSwitch77 = staticSwitch106;
				#endif
				
				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				
				float3 Albedo = staticSwitch77.rgb;
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormals" }

			ZWrite On
			Blend One Zero
            ZTest LEqual
            ZWrite On

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float3 worldNormal : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal( v.ase_normal );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.worldNormal = normalWS;

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				
				return float4(PackNormalOctRectEncode(TransformWorldToViewDir(IN.worldNormal, true)), 0.0, 0.0);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "GBuffer"
			Tags { "LightMode"="UniversalGBuffer" }
			
			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x 

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			#pragma multi_compile _ _GBUFFER_NORMALS_OCT
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_SCREEN_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _DISPLACEMENTTOGGLE_ON
			#pragma shader_feature_local _REFRACTION_ON
			#pragma shader_feature_local _FOAMTOGGLE_ON
			#pragma shader_feature_local _DISTANCEFADETOGGLE_ON
			#pragma shader_feature_local _FADEFROM_CAMERA _FADEFROM_WATER_CENTER


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			half4 _Color_Deep;
			half4 _Color_Shallow;
			half _DisplacementFrequency;
			half _Opacity;
			half _Smoothness;
			half _Metallic;
			half _EdgePower;
			half _FoamScale;
			half _HighLightDensity;
			half _Depth;
			half _Normal_DirectionAngle_02;
			half _FadeDistance;
			half _NormalStrength;
			half _MipMapBias;
			half _NormalMapTiling;
			half _Normal_DirectionAngle_01;
			half _NormalSpeed;
			half _DisplacementAmplitude;
			half _RefractionStrenght;
			half _DistanceFadeHardness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _NormalMap_01;
			sampler2D _NormalMap_02;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				half3 objToWorldDir137 = mul( GetObjectToWorldMatrix(), float4( v.vertex.xyz, 0 ) ).xyz;
				half temp_output_141_0 = sin( ( _TimeParameters.x * TWO_PI * _DisplacementFrequency ) );
				half3 appendResult123 = (half3(0.0 , ( objToWorldDir137.y + ( temp_output_141_0 * _DisplacementAmplitude ) ) , 0.0));
				half3 worldToObjDir135 = mul( GetWorldToObjectMatrix(), float4( appendResult123, 0 ) ).xyz;
				#ifdef _DISPLACEMENTTOGGLE_ON
				half3 staticSwitch148 = worldToObjDir135;
				#else
				half3 staticSwitch148 = float3( 0,0,0 );
				#endif
				
				float3 objectToViewPos = TransformWorldToView(TransformObjectToWorld(v.vertex.xyz));
				float eyeDepth = -objectToViewPos.z;
				o.ase_texcoord7.z = eyeDepth;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.w = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = staticSwitch148;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			FragmentOutput frag ( VertexOutput IN 
								#ifdef ASE_DEPTH_WRITE_ON
								,out float outputDepth : ASE_SV_DEPTH
								#endif
								 )
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				half mulTime14 = _TimeParameters.x * _NormalSpeed;
				half temp_output_30_0 = radians( _Normal_DirectionAngle_01 );
				half2 appendResult26 = (half2(sin( temp_output_30_0 ) , cos( temp_output_30_0 )));
				half2 temp_cast_0 = (_NormalMapTiling).xx;
				half2 texCoord20 = IN.ase_texcoord7.xy * temp_cast_0 + float2( 0,0 );
				half2 panner11 = ( mulTime14 * appendResult26 + texCoord20);
				half temp_output_181_0 = ( 1.0 - _NormalStrength );
				half3 lerpResult179 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_01, float4( panner11, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0139 = lerpResult179;
				half temp_output_34_0 = radians( _Normal_DirectionAngle_02 );
				half2 appendResult32 = (half2(sin( temp_output_34_0 ) , cos( temp_output_34_0 )));
				half2 panner12 = ( mulTime14 * appendResult32 + texCoord20);
				half3 lerpResult180 = lerp( UnpackNormalScale( tex2Dbias( _NormalMap_02, float4( panner12, 0, _MipMapBias) ), 1.0f ) , float3( 0,0,1 ) , temp_output_181_0);
				half3 xyzNormalMap_0240 = lerpResult180;
				half3 BlendedPannedNormalMaps37 = BlendNormal( xyzNormalMap_0139 , xyzNormalMap_0240 );
				float eyeDepth = IN.ase_texcoord7.z;
				half eyeDepth28_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ase_screenPosNorm.xy ),_ZBufferParams);
				half2 temp_output_20_0_g1 = ( (BlendedPannedNormalMaps37).xy * ( _RefractionStrenght / max( eyeDepth , 0.1 ) ) * saturate( ( eyeDepth28_g1 - eyeDepth ) ) );
				half eyeDepth2_g1 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( ( half4( temp_output_20_0_g1, 0.0 , 0.0 ) + ase_screenPosNorm ).xy ),_ZBufferParams);
				half2 temp_output_32_0_g1 = (( half4( ( temp_output_20_0_g1 * saturate( ( eyeDepth2_g1 - eyeDepth ) ) ), 0.0 , 0.0 ) + ase_screenPosNorm )).xy;
				half2 temp_output_74_38 = temp_output_32_0_g1;
				#ifdef _REFRACTION_ON
				half4 staticSwitch100 = half4( temp_output_74_38, 0.0 , 0.0 );
				#else
				half4 staticSwitch100 = ase_screenPosNorm;
				#endif
				half eyeDepth99 = LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH( staticSwitch100.xy ),_ZBufferParams);
				half temp_output_82_0 = ( eyeDepth99 - eyeDepth );
				half depth69 = ( temp_output_82_0 * _Depth );
				half temp_output_105_0 = saturate( depth69 );
				half4 lerpResult104 = lerp( _Color_Shallow , _Color_Deep , temp_output_105_0);
				#ifdef _FOAMTOGGLE_ON
				half4 staticSwitch106 = lerpResult104;
				#else
				half4 staticSwitch106 = _Color_Deep;
				#endif
				half4 fetchOpaqueVal75 = float4( SHADERGRAPH_SAMPLE_SCENE_COLOR( temp_output_74_38 ), 1.0 );
				half4 lerpResult172 = lerp( _Color_Deep , fetchOpaqueVal75 , temp_output_105_0);
				#ifdef _REFRACTION_ON
				half4 staticSwitch77 = ( lerpResult104 + lerpResult172 );
				#else
				half4 staticSwitch77 = staticSwitch106;
				#endif
				
				half3 normalizeResult45 = normalize( xyzNormalMap_0139 );
				half3 normalizeResult46 = normalize( xyzNormalMap_0240 );
				half dotResult53 = dot( normalizeResult45 , normalizeResult46 );
				half dotResult47 = dot( xyzNormalMap_0139.x , xyzNormalMap_0240.x );
				half clampResult48 = clamp( dotResult47 , 0.0 , 1.0 );
				half clampResult52 = clamp( ( pow( clampResult48 , ( 1.0 - _HighLightDensity ) ) * 3.3 ) , 0.0 , 1.0 );
				half NormalHighlights59 = ( ( pow( abs( ( ( dotResult53 - 0.005 ) * clampResult52 ) ) , 3800.0 ) * 50000000.0 ) * 5000.0 );
				#ifdef _FOAMTOGGLE_ON
				half staticSwitch97 = ( ( 1.0 - saturate( ( temp_output_82_0 * 12.0 ) ) ) + ( ( 1.0 - saturate( ( temp_output_82_0 + _FoamScale ) ) ) * _EdgePower ) );
				#else
				half staticSwitch97 = 0.0;
				#endif
				half3 temp_cast_5 = (( NormalHighlights59 + staticSwitch97 )).xxx;
				
				half4 transform163 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				#if defined(_FADEFROM_CAMERA)
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#elif defined(_FADEFROM_WATER_CENTER)
				half4 staticSwitch160 = transform163;
				#else
				half4 staticSwitch160 = half4( _WorldSpaceCameraPos , 0.0 );
				#endif
				half4 temp_output_5_0_g8 = ( ( half4( WorldPosition , 0.0 ) - staticSwitch160 ) / _FadeDistance );
				half dotResult8_g8 = dot( temp_output_5_0_g8 , temp_output_5_0_g8 );
				half distanceFade168 = ( 1.0 - pow( saturate( dotResult8_g8 ) , _DistanceFadeHardness ) );
				#ifdef _DISTANCEFADETOGGLE_ON
				half staticSwitch167 = ( _Opacity * distanceFade168 );
				#else
				half staticSwitch167 = _Opacity;
				#endif
				
				float3 Albedo = staticSwitch77.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = temp_cast_5;
				float3 Specular = 0.5;
				float Metallic = 0;
				float Smoothness = 0.5;
				float Occlusion = 1;
				float Alpha = staticSwitch167;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif

				BRDFData brdfData;
				InitializeBRDFData( Albedo, Metallic, Specular, Smoothness, Alpha, brdfData);
				half4 color;
				color.rgb = GlobalIllumination( brdfData, inputData.bakedGI, Occlusion, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif
				
				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
				
					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif
				
				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, WorldNormal ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif
				
				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif
				
				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				
				return BRDFDataToGbuffer(brdfData, inputData, Smoothness, Emission + color.rgb);
			}

			ENDHLSL
		}
		
	}
	/*ase_lod*/
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18912
3177.333;58.66667;2206.667;1231.667;3478.555;-3.893555;1;True;True
Node;AmplifyShaderEditor.RangedFloatNode;31;-3410.605,720.0374;Inherit;False;Property;_Normal_DirectionAngle_02;Normal_DirectionAngle_02;3;0;Create;True;0;0;0;False;0;False;45;80;45;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;25;-3412.295,538.5341;Inherit;False;Property;_Normal_DirectionAngle_01;Normal_DirectionAngle_01;1;1;[Header];Create;True;1;Normal directions in degrees;0;0;False;0;False;0;19;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RadiansOpNode;30;-3137.364,525.1895;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RadiansOpNode;34;-3120.364,739.0753;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;13;-3412.729,625.2398;Inherit;False;Property;_NormalSpeed;NormalSpeed;5;0;Create;True;0;0;0;False;0;False;0.1;0.036;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.CosOpNode;28;-2986.986,559.5002;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;27;-2988.985,492.5002;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;21;-3346.033,396.8344;Inherit;False;Property;_NormalMapTiling;NormalMapTiling;4;0;Create;True;0;0;0;False;0;False;10;101.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;35;-2984.467,717.1824;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CosOpNode;33;-2982.467,781.1824;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;32;-2851.5,723.77;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;26;-2847.659,508.1949;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleTimeNode;14;-2879.04,631.2398;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;20;-2947.02,366.3543;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;63;-2656.589,366.8228;Inherit;False;Property;_NormalStrength;NormalStrength;6;0;Create;True;0;0;0;False;0;False;1;0.274;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.PannerNode;12;-2646.432,707.0024;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PannerNode;11;-2644.731,508.5871;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;65;-2744.84,864.4788;Inherit;False;Property;_MipMapBias;MipMapBias;7;0;Create;True;0;0;0;False;0;False;1;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;10;-2365.884,677.319;Inherit;True;Property;_NormalMap_02;NormalMap_02;2;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;-1;c1015f83af99519488c2a667e2254432;743131242bccd9e4ea0bc40e9d9b9840;True;0;True;bump;Auto;True;Object;-1;MipBias;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;181;-2315.016,342.4766;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;9;-2359.023,474.5379;Inherit;True;Property;_NormalMap_01;NormalMap_01;0;3;[Header];[NoScaleOffset];[Normal];Create;True;1;Water Normals;0;0;False;0;False;-1;339f9ba1f96226b4288643740f92d8b3;81bf2db950be59b4e842a55e5f804b7f;True;0;True;bump;Auto;True;Object;-1;MipBias;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;179;-1977.258,293.6051;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,1;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;180;-2023.958,728.0184;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,1;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;39;-1901.906,463.7174;Inherit;False;xyzNormalMap_01;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;40;-1864.808,761.0405;Inherit;False;xyzNormalMap_02;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;42;-2893.107,-1081.853;Inherit;False;40;xyzNormalMap_02;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;41;-2948.295,-1255.907;Inherit;False;39;xyzNormalMap_01;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BreakToComponentsNode;44;-2643.211,-1026.569;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.BreakToComponentsNode;43;-2655.705,-1264.776;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.BlendNormalsNode;18;-1739.101,610.3459;Inherit;False;0;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DotProductOpNode;47;-2459.211,-1050.569;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;37;-1525.838,610.6121;Inherit;False;BlendedPannedNormalMaps;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;50;-2691.955,-799.572;Inherit;False;Property;_HighLightDensity;HighLightDensity;8;0;Create;True;0;0;0;False;0;False;1;0.698;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;76;-1012.136,148.7339;Inherit;False;Property;_RefractionStrenght;RefractionStrenght;16;0;Create;True;0;0;0;False;0;False;0;0.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;48;-2330.211,-1022.569;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;38;-1235.677,-24.20086;Inherit;False;37;BlendedPannedNormalMaps;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OneMinusNode;176;-2330.83,-762.4709;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;49;-2152.342,-1023.328;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;74;-798.5625,130.5801;Inherit;False;DepthMaskedRefraction;-1;;1;c805f061214177c42bca056464193f81;2,40,0,103,0;2;35;FLOAT3;0,0,0;False;37;FLOAT;0.02;False;1;FLOAT2;38
Node;AmplifyShaderEditor.NormalizeNode;45;-2651.211,-1335.569;Inherit;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ScreenPosInputsNode;101;-1049.908,517.5394;Float;False;0;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.NormalizeNode;46;-2656.211,-1108.869;Inherit;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;51;-1967.602,-1037.944;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;3.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;100;-853.2204,587.9753;Inherit;False;Property;_Refraction;Refraction;17;0;Create;True;0;0;0;False;0;False;0;1;0;True;;Toggle;2;Key0;Key1;Reference;77;True;True;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DotProductOpNode;53;-2454.211,-1215.569;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SurfaceDepthNode;81;-902.3278,932.3751;Inherit;False;0;1;0;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenDepthNode;99;-819.9956,816.5585;Inherit;False;0;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;52;-1831.602,-1036.944;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;54;-2230.211,-1215.569;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.005;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;103;-580.5901,700.1691;Inherit;False;Property;_Depth;Depth;19;0;Create;True;0;0;0;False;0;False;1;0.119;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.WorldSpaceCameraPos;151;756.2316,-884.5854;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;68;-635.4913,1078.9;Inherit;False;Property;_FoamScale;FoamScale;14;1;[Header];Create;True;1;Depth;0;0;False;0;False;1;0.2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;122;812.6372,948.2632;Inherit;False;Property;_DisplacementFrequency;DisplacementFrequency;20;1;[Header];Create;True;1;Displacement;0;0;False;0;False;1;0.157;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;142;877.8467,854.9695;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;55;-1831.211,-1217.569;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;82;-583.9279,845.975;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TauNode;140;1057.847,920.9695;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;163;808.4547,-731.0248;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;143;1173.847,892.9695;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;84;-244.2117,1040.703;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;160;1123.054,-809.0251;Inherit;False;Property;_FadeFrom;FadeFrom;25;0;Create;True;0;0;0;False;0;False;0;0;1;True;;KeywordEnum;2;camera;water_center;Create;True;True;9;1;FLOAT4;0,0,0,0;False;0;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;4;FLOAT4;0,0,0,0;False;5;FLOAT4;0,0,0,0;False;6;FLOAT4;0,0,0,0;False;7;FLOAT4;0,0,0,0;False;8;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;155;1161.744,-536.0029;Inherit;False;Property;_DistanceFadeHardness;DistanceFadeHardness;24;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;88;-227.3118,936.7036;Inherit;False;Constant;_Float3;Float 3;16;0;Create;True;0;0;0;False;0;False;12;12;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;102;-407.8237,754.657;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.AbsOpNode;175;-1701.83,-1230.471;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;153;1173.187,-633.1509;Inherit;False;Property;_FadeDistance;FadeDistance;23;1;[Header];Create;True;1;Fade distant water;0;0;False;0;False;1;467.4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;130;1026.847,564.9695;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;87;-72.61179,849.6035;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;85;-92.11187,1039.403;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;141;1306.847,880.9695;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;56;-1591.211,-1218.569;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;3800;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;154;1433.445,-675.103;Inherit;True;SphereMask;-1;;8;988803ee12caf5f4690caee3c8c4a5bb;0;3;15;FLOAT4;0,0,0,0;False;14;FLOAT;0;False;12;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;69;-182.6771,629.3035;Inherit;False;depth;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;146;1239.847,1034.969;Inherit;False;Property;_DisplacementAmplitude;DisplacementAmplitude;21;0;Create;True;0;0;0;False;0;False;0.1;0.069;0;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;95;255.4874,1092.704;Inherit;False;Property;_EdgePower;EdgePower;15;0;Create;True;0;0;0;False;0;False;0.576821;0.259;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;86;67.7882,1042.004;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;157;1758.754,-619.2247;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;89;72.98822,850.9036;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;57;-1418.211,-1217.569;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;5E+07;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;70;-766.1791,-104.8531;Inherit;False;69;depth;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;147;1507.847,984.9695;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;137;1221.847,564.9695;Inherit;False;Object;World;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ColorNode;71;-666.709,-366.0005;Inherit;False;Property;_Color_Deep;Color_Deep;13;0;Create;True;0;0;0;False;0;False;0.3221193,0.4825942,0.8396226,0;0.03181737,0.1226411,0.1150724,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;19;-667.9901,-544.446;Inherit;False;Property;_Color_Shallow;Color_Shallow;12;0;Create;True;0;0;0;False;0;False;0.3221193,0.4825942,0.8396226,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;127;1417.847,701.9695;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;168;1923.854,-621.8247;Inherit;False;distanceFade;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;105;-571.6408,-97.2246;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenColorNode;75;-441.7536,130.442;Inherit;False;Global;_GrabScreen0;Grab Screen 0;14;0;Create;True;0;0;0;False;0;False;Object;-1;False;False;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;58;-1267.211,-1215.569;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;5000;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;94;608.9875,931.4033;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;90;219.1881,850.1036;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;172;-155.0251,130.8271;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;123;1511.847,576.9695;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;170;986.151,147.7672;Inherit;False;Property;_Opacity;Opacity;11;0;Create;True;0;0;0;False;0;False;1;0.667;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;96;602.1874,659.5035;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;169;949.511,304.7406;Inherit;False;168;distanceFade;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;59;-1057.152,-1196.661;Inherit;False;NormalHighlights;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;104;-186.4674,-234.5439;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;78;159.79,-14.89779;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;97;499.7334,495.3215;Inherit;False;Property;_FoamToggle;FoamToggle;18;0;Create;True;0;0;0;False;0;False;0;1;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;171;1168.151,285.7672;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;135;1639.847,552.9695;Inherit;False;World;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;60;134.3007,181.4827;Inherit;False;59;NormalHighlights;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;106;101.7589,-290.9246;Inherit;False;Property;_Refraction;Refraction;18;0;Create;True;0;0;0;False;0;False;0;1;0;True;;Toggle;2;Key0;Key1;Reference;97;True;True;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;148;1320.847,415.9695;Inherit;False;Property;_DisplacementToggle;DisplacementToggle;22;0;Create;True;0;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;167;1335.054,228.8752;Inherit;False;Property;_DistanceFadeToggle;DistanceFadeToggle;26;0;Create;True;0;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;145;1462.847,845.9695;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;24;128.1564,258.7738;Inherit;False;Property;_Metallic;Metallic;9;1;[Header];Create;True;1;PBR Values;0;0;False;0;False;0;0.078;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;77;404.2843,-42.81118;Inherit;False;Property;_Refraction;Refraction;17;0;Create;True;0;0;0;False;0;False;0;1;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StickyNoteNode;36;-3477.092,247.5993;Inherit;False;2221.04;720.1423;NormalMapPanning;Normals;0.2327043,0.7003093,1,1;;0;0
Node;AmplifyShaderEditor.RangedFloatNode;23;128.1564,331.7739;Inherit;False;Property;_Smoothness;Smoothness;10;0;Create;True;0;0;0;False;0;False;0.81;0.965;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;91;497.2275,330.442;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=DepthNormals;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalGBuffer;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;False;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;1848.457,40.56159;Half;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;S_kijai_water_simple;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;2;False;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;1;Surface;1;  Refraction Model;0;  Blend;0;Two Sided;1;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Write Depth;0;  Early Z;0;Vertex Position,InvertActionOnDeselection;1;0;8;False;True;False;True;True;True;True;True;False;;False;0
WireConnection;30;0;25;0
WireConnection;34;0;31;0
WireConnection;28;0;30;0
WireConnection;27;0;30;0
WireConnection;35;0;34;0
WireConnection;33;0;34;0
WireConnection;32;0;35;0
WireConnection;32;1;33;0
WireConnection;26;0;27;0
WireConnection;26;1;28;0
WireConnection;14;0;13;0
WireConnection;20;0;21;0
WireConnection;12;0;20;0
WireConnection;12;2;32;0
WireConnection;12;1;14;0
WireConnection;11;0;20;0
WireConnection;11;2;26;0
WireConnection;11;1;14;0
WireConnection;10;1;12;0
WireConnection;10;2;65;0
WireConnection;181;0;63;0
WireConnection;9;1;11;0
WireConnection;9;2;65;0
WireConnection;179;0;9;0
WireConnection;179;2;181;0
WireConnection;180;0;10;0
WireConnection;180;2;181;0
WireConnection;39;0;179;0
WireConnection;40;0;180;0
WireConnection;44;0;42;0
WireConnection;43;0;41;0
WireConnection;18;0;39;0
WireConnection;18;1;40;0
WireConnection;47;0;43;0
WireConnection;47;1;44;0
WireConnection;37;0;18;0
WireConnection;48;0;47;0
WireConnection;176;0;50;0
WireConnection;49;0;48;0
WireConnection;49;1;176;0
WireConnection;74;35;38;0
WireConnection;74;37;76;0
WireConnection;45;0;41;0
WireConnection;46;0;42;0
WireConnection;51;0;49;0
WireConnection;100;1;101;0
WireConnection;100;0;74;38
WireConnection;53;0;45;0
WireConnection;53;1;46;0
WireConnection;99;0;100;0
WireConnection;52;0;51;0
WireConnection;54;0;53;0
WireConnection;55;0;54;0
WireConnection;55;1;52;0
WireConnection;82;0;99;0
WireConnection;82;1;81;0
WireConnection;143;0;142;0
WireConnection;143;1;140;0
WireConnection;143;2;122;0
WireConnection;84;0;82;0
WireConnection;84;1;68;0
WireConnection;160;1;151;0
WireConnection;160;0;163;0
WireConnection;102;0;82;0
WireConnection;102;1;103;0
WireConnection;175;0;55;0
WireConnection;87;0;82;0
WireConnection;87;1;88;0
WireConnection;85;0;84;0
WireConnection;141;0;143;0
WireConnection;56;0;175;0
WireConnection;154;15;160;0
WireConnection;154;14;153;0
WireConnection;154;12;155;0
WireConnection;69;0;102;0
WireConnection;86;0;85;0
WireConnection;157;0;154;0
WireConnection;89;0;87;0
WireConnection;57;0;56;0
WireConnection;147;0;141;0
WireConnection;147;1;146;0
WireConnection;137;0;130;0
WireConnection;127;0;137;2
WireConnection;127;1;147;0
WireConnection;168;0;157;0
WireConnection;105;0;70;0
WireConnection;75;0;74;38
WireConnection;58;0;57;0
WireConnection;94;0;86;0
WireConnection;94;1;95;0
WireConnection;90;0;89;0
WireConnection;172;0;71;0
WireConnection;172;1;75;0
WireConnection;172;2;105;0
WireConnection;123;1;127;0
WireConnection;96;0;90;0
WireConnection;96;1;94;0
WireConnection;59;0;58;0
WireConnection;104;0;19;0
WireConnection;104;1;71;0
WireConnection;104;2;105;0
WireConnection;78;0;104;0
WireConnection;78;1;172;0
WireConnection;97;0;96;0
WireConnection;171;0;170;0
WireConnection;171;1;169;0
WireConnection;135;0;123;0
WireConnection;106;1;71;0
WireConnection;106;0;104;0
WireConnection;148;0;135;0
WireConnection;167;1;170;0
WireConnection;167;0;171;0
WireConnection;145;0;141;0
WireConnection;145;1;146;0
WireConnection;77;1;106;0
WireConnection;77;0;78;0
WireConnection;91;0;60;0
WireConnection;91;1;97;0
WireConnection;1;0;77;0
WireConnection;1;1;38;0
WireConnection;1;2;91;0
WireConnection;1;3;24;0
WireConnection;1;4;23;0
WireConnection;1;6;167;0
WireConnection;1;8;148;0
ASEEND*/
//CHKSM=A16C7B02B13EFB28A579CB99D7D1FD0C3FE2B328