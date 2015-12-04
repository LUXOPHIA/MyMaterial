unit LUX.DirectX.d3d11shader;

interface //#################################################################### Å°

uses WinApi.Windows,
     LUX.DirectX.d3dcommon;

type T_INT = Integer;

//////////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) Microsoft Corporation.  All rights reserved.
//
//  File:       D3D11Shader.h
//  Content:    D3D11 Shader Types and APIs
//
//////////////////////////////////////////////////////////////////////////////

type T_D3D11_SHADER_VERSION_TYPE = ( D3D11_SHVER_PIXEL_SHADER    = 0,
                                     D3D11_SHVER_VERTEX_SHADER   = 1,
                                     D3D11_SHVER_GEOMETRY_SHADER = 2,
    
                                     // D3D11 Shaders
                                     D3D11_SHVER_HULL_SHADER     = 3,
                                     D3D11_SHVER_DOMAIN_SHADER   = 4,
                                     D3D11_SHVER_COMPUTE_SHADER  = 5,

                                     D3D11_SHVER_RESERVED0       = $FFF0 );

// Slot ID for library function return
const D3D_RETURN_PARAMETER_INDEX = -1;

type T_D3D11_RESOURCE_RETURN_TYPE = T_D3D_RESOURCE_RETURN_TYPE;

type T_D3D11_CBUFFER_TYPE = T_D3D_CBUFFER_TYPE;

type T_D3D11_SIGNATURE_PARAMETER_DESC = record
       SemanticName    :LPCSTR;                         // Name of the semantic
       SemanticIndex   :UINT;                           // Index of the semantic
       Register_       :UINT;                           // Number of member variables
       SystemValueType :T_D3D_NAME;                     // A predefined system value, or D3D_NAME_UNDEFINED if not applicable
       ComponentType   :T_D3D_REGISTER_COMPONENT_TYPE;  // Scalar type (e.g. uint, float, etc.)
       Mask            :BYTE;                           // Mask to indicate which components of the register
                                                        // are used (combination of D3D10_COMPONENT_MASK values)
       ReadWriteMask   :BYTE;                           // Mask to indicate whether a given component is
                                                        // never written (if this is an output signature) or
                                                        // always read (if this is an input signature).
                                                        // (combination of D3D_MASK_* values)
       Stream          :UINT;                           // Stream index
       MinPrecision    :T_D3D_MIN_PRECISION;            // Minimum desired interpolation precision
     end;

type T_D3D11_SHADER_BUFFER_DESC = record
       Name      :LPCSTR;              // Name of the constant buffer
       Type_     :T_D3D_CBUFFER_TYPE;  // Indicates type of buffer content
       Variables :UINT;                // Number of member variables
       Size      :UINT;                // Size of CB (in bytes)
      uFlags     :UINT;                // Buffer description flags
     end;

type T_D3D11_SHADER_VARIABLE_DESC = record
       Name         :LPCSTR;  // Name of the variable
       StartOffset  :UINT;    // Offset in constant buffer's backing store
       Size         :UINT;    // Size of variable (in bytes)
      uFlags        :UINT;    // Variable flags
       DefaultValue :LPVOID;  // Raw pointer to default value
       StartTexture :UINT;    // First texture index (or -1 if no textures used)
       TextureSize  :UINT;    // Number of texture slots possibly used.
       StartSampler :UINT;    // First sampler index (or -1 if no textures used)
       SamplerSize  :UINT;    // Number of sampler slots possibly used.
     end;

type T_D3D11_SHADER_TYPE_DESC = record
       Class_   :T_D3D_SHADER_VARIABLE_CLASS;  // Variable class (e.g. object, matrix, etc.)
       Type_    :T_D3D_SHADER_VARIABLE_TYPE;   // Variable type (e.g. float, sampler, etc.)
       Rows     :UINT;                         // Number of rows (for matrices, 1 for other numeric, 0 if not applicable)
       Columns  :UINT;                         // Number of columns (for vectors & matrices, 1 for other numeric, 0 if not applicable)
       Elements :UINT;                         // Number of elements (0 if not an array)
       Members  :UINT;                         // Number of members (0 if not a structure)
       Offset   :UINT;                         // Offset from the start of structure (0 if not a structure member)
       Name     :LPCSTR;                       // Name of type, can be NULL
     end;

type T_D3D11_TESSELLATOR_DOMAIN = T_D3D_TESSELLATOR_DOMAIN;

type T_D3D11_TESSELLATOR_PARTITIONING = T_D3D_TESSELLATOR_PARTITIONING;

type T_D3D11_TESSELLATOR_OUTPUT_PRIMITIVE = T_D3D_TESSELLATOR_OUTPUT_PRIMITIVE;

type T_D3D11_SHADER_DESC = record
       Version                     :UINT;                                // Shader version
       Creator                     :LPCSTR;                              // Creator string
       Flags                       :UINT;                                // Shader compilation/parse flags

       ConstantBuffers             :UINT;                                // Number of constant buffers
       BoundResources              :UINT;                                // Number of bound resources
       InputParameters             :UINT;                                // Number of parameters in the input signature
       OutputParameters            :UINT;                                // Number of parameters in the output signature

       InstructionCount            :UINT;                                // Number of emitted instructions
       TempRegisterCount           :UINT;                                // Number of temporary registers used
       TempArrayCount              :UINT;                                // Number of temporary arrays used
       DefCount                    :UINT;                                // Number of constant defines
       DclCount                    :UINT;                                // Number of declarations (input + output)
       TextureNormalInstructions   :UINT;                                // Number of non-categorized texture instructions
       TextureLoadInstructions     :UINT;                                // Number of texture load instructions
       TextureCompInstructions     :UINT;                                // Number of texture comparison instructions
       TextureBiasInstructions     :UINT;                                // Number of texture bias instructions
       TextureGradientInstructions :UINT;                                // Number of texture gradient instructions
       FloatInstructionCount       :UINT;                                // Number of floating point arithmetic instructions used
       IntInstructionCount         :UINT;                                // Number of signed integer arithmetic instructions used
       UintInstructionCount        :UINT;                                // Number of unsigned integer arithmetic instructions used
       StaticFlowControlCount      :UINT;                                // Number of static flow control instructions used
       DynamicFlowControlCount     :UINT;                                // Number of dynamic flow control instructions used
       MacroInstructionCount       :UINT;                                // Number of macro instructions used
       ArrayInstructionCount       :UINT;                                // Number of array instructions used
       CutInstructionCount         :UINT;                                // Number of cut instructions used
       EmitInstructionCount        :UINT;                                // Number of emit instructions used
       GSOutputTopology            :T_D3D_PRIMITIVE_TOPOLOGY;            // Geometry shader output topology
       GSMaxOutputVertexCount      :UINT;                                // Geometry shader maximum output vertex count
       InputPrimitive              :T_D3D_PRIMITIVE;                     // GS/HS input primitive
       PatchConstantParameters     :UINT;                                // Number of parameters in the patch constant signature
      cGSInstanceCount             :UINT;                                // Number of Geometry shader instances
      cControlPoints               :UINT;                                // Number of control points in the HS->DS stage
       HSOutputPrimitive           :T_D3D_TESSELLATOR_OUTPUT_PRIMITIVE;  // Primitive output by the tessellator
       HSPartitioning              :T_D3D_TESSELLATOR_PARTITIONING;      // Partitioning mode of the tessellator
       TessellatorDomain           :T_D3D_TESSELLATOR_DOMAIN;            // Domain of the tessellator (quad, tri, isoline)
       // instruction counts
      cBarrierInstructions         :UINT;                                // Number of barrier instructions in a compute shader
      cInterlockedInstructions     :UINT;                                // Number of interlocked instructions
      cTextureStoreInstructions    :UINT;                                // Number of texture writes
     end;

type T_D3D11_SHADER_INPUT_BIND_DESC = record
       Name       :LPCSTR;                      // Name of the resource
       Type_      :T_D3D_SHADER_INPUT_TYPE;     // Type of resource (e.g. texture, cbuffer, etc.)
       BindPoint  :UINT;                        // Starting bind point
       BindCount  :UINT;                        // Number of contiguous bind points (for arrays)

      uFlags      :UINT;                        // Input binding flags
       ReturnType :T_D3D_RESOURCE_RETURN_TYPE;  // Return type (if texture)
       Dimension  :T_D3D_SRV_DIMENSION;         // Dimension (if texture)
       NumSamples :UINT;                        // Number of samples (0 if not MS texture)
     end;

const D3D_SHADER_REQUIRES_DOUBLES                      = $00000001;
const D3D_SHADER_REQUIRES_EARLY_DEPTH_STENCIL          = $00000002;
const D3D_SHADER_REQUIRES_UAVS_AT_EVERY_STAGE          = $00000004;
const D3D_SHADER_REQUIRES_64_UAVS                      = $00000008;
const D3D_SHADER_REQUIRES_MINIMUM_PRECISION            = $00000010;
const D3D_SHADER_REQUIRES_11_1_DOUBLE_EXTENSIONS       = $00000020;
const D3D_SHADER_REQUIRES_11_1_SHADER_EXTENSIONS       = $00000040;
const D3D_SHADER_REQUIRES_LEVEL_9_COMPARISON_FILTERING = $00000080;
const D3D_SHADER_REQUIRES_TILED_RESOURCES              = $00000100;

type T_D3D11_LIBRARY_DESC = record
       Creator       :LPCSTR;  // The name of the originator of the library.
       Flags         :UINT;    // Compilation flags.
       FunctionCount :UINT;    // Number of functions exported from the library.
     end;

type T_D3D11_FUNCTION_DESC = record
       Version                     :UINT;                 // Shader version
       Creator                     :LPCSTR;               // Creator string
       Flags                       :UINT;                 // Shader compilation/parse flags

       ConstantBuffers             :UINT;                 // Number of constant buffers
       BoundResources              :UINT;                 // Number of bound resources

       InstructionCount            :UINT;                 // Number of emitted instructions
       TempRegisterCount           :UINT;                 // Number of temporary registers used
       TempArrayCount              :UINT;                 // Number of temporary arrays used
       DefCount                    :UINT;                 // Number of constant defines
       DclCount                    :UINT;                 // Number of declarations (input + output)
       TextureNormalInstructions   :UINT;                 // Number of non-categorized texture instructions
       TextureLoadInstructions     :UINT;                 // Number of texture load instructions
       TextureCompInstructions     :UINT;                 // Number of texture comparison instructions
       TextureBiasInstructions     :UINT;                 // Number of texture bias instructions
       TextureGradientInstructions :UINT;                 // Number of texture gradient instructions
       FloatInstructionCount       :UINT;                 // Number of floating point arithmetic instructions used
       IntInstructionCount         :UINT;                 // Number of signed integer arithmetic instructions used
       UintInstructionCount        :UINT;                 // Number of unsigned integer arithmetic instructions used
       StaticFlowControlCount      :UINT;                 // Number of static flow control instructions used
       DynamicFlowControlCount     :UINT;                 // Number of dynamic flow control instructions used
       MacroInstructionCount       :UINT;                 // Number of macro instructions used
       ArrayInstructionCount       :UINT;                 // Number of array instructions used
       MovInstructionCount         :UINT;                 // Number of mov instructions used
       MovcInstructionCount        :UINT;                 // Number of movc instructions used
       ConversionInstructionCount  :UINT;                 // Number of type conversion instructions used
       BitwiseInstructionCount     :UINT;                 // Number of bitwise arithmetic instructions used
       MinFeatureLevel             :T_D3D_FEATURE_LEVEL;  // Min target of the function byte code
       RequiredFeatureFlags        :UINT64;               // Required feature flags

       Name                        :LPCSTR;               // Function name
       FunctionParameterCount      :T_INT;                // Number of logical parameters in the function signature (not including return)
       HasReturn                   :BOOL;                 // TRUE, if function returns a value, false - it is a subroutine
       Has10Level9VertexShader     :BOOL;                 // TRUE, if there is a 10L9 VS blob
       Has10Level9PixelShader      :BOOL;                 // TRUE, if there is a 10L9 PS blob
     end;

type P_D3D11_PARAMETER_DESC = ^T_D3D11_PARAMETER_DESC;

     T_D3D11_PARAMETER_DESC = record
       Name              :LPCSTR;                       // Parameter name.
       SemanticName      :LPCSTR;                       // Parameter semantic name (+index).
       Type_             :T_D3D_SHADER_VARIABLE_TYPE;   // Element type.
       Class_            :T_D3D_SHADER_VARIABLE_CLASS;  // Scalar/Vector/Matrix.
       Rows              :UINT;                         // Rows are for matrix parameters.
       Columns           :UINT;                         // Components or Columns in matrix.
       InterpolationMode :T_D3D_INTERPOLATION_MODE;     // Interpolation mode.
       Flags             :T_D3D_PARAMETER_FLAGS;        // Parameter modifiers.

       FirstInRegister   :UINT;                         // The first input register for this parameter.
       FirstInComponent  :UINT;                         // The first input register component for this parameter.
       FirstOutRegister  :UINT;                         // The first output register for this parameter.
       FirstOutComponent :UINT;                         // The first output register component for this parameter.
     end;

//////////////////////////////////////////////////////////////////////////////
// Interfaces ////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ÅyíËêîÅz

      IID_ID3D11ShaderReflectionType           :TGUID = '{6E6FFA6A-9BAE-4613-A51E-91652D508C21}';
      IID_ID3D11ShaderReflectionVariable       :TGUID = '{51F23923-F3E5-4BD1-91CB-606177D8DB4C}';
      IID_ID3D11ShaderReflectionConstantBuffer :TGUID = '{EB62D63D-93DD-4318-8AE8-C6F83AD371B8}';
      IID_ID3D11ShaderReflection               :TGUID = '{8d536ca1-0cca-4956-a837-786963755584}';
      IID_ID3D11LibraryReflection              :TGUID = '{54384F1B-5B3E-4BB7-AE01-60BA3097CBB6}';
      IID_ID3D11FunctionReflection             :TGUID = '{207BCECB-D683-4A06-A8A3-9B149B9F73A4}';
      IID_ID3D11FunctionParameterReflection    :TGUID = '{42757488-334F-47FE-982E-1A65D08CC462}';
      IID_ID3D11Module                         :TGUID = '{CAC701EE-80FC-4122-8242-10B39C8CEC34}';
      IID_ID3D11ModuleInstance                 :TGUID = '{469E07F7-045A-48D5-AA12-68A478CDF75D}';
      IID_ID3D11Linker                         :TGUID = '{59A6CD0E-E10D-4C1F-88C0-63ABA1DAF30E}';
      IID_ID3D11LinkingNode                    :TGUID = '{D80DD70C-8D2F-4751-94A1-03C79B3556DB}';
      IID_ID3D11FunctionLinkingGraph           :TGUID = '{54133220-1CE8-43D3-8236-9855C5CEECFF}';

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Åyå^Åz

     T_ID3D11ShaderReflectionType           = interface;
     T_ID3D11ShaderReflectionVariable       = interface;
     T_ID3D11ShaderReflectionConstantBuffer = interface;
     T_ID3D11ShaderReflection               = interface;
     T_ID3D11LibraryReflection              = interface;
     T_ID3D11FunctionReflection             = interface;
     T_ID3D11FunctionParameterReflection    = interface;
     T_ID3D11ModuleInstance                 = interface;

     T_LPD3D11SHADERREFLECTIONTYPE           = ^T_ID3D11ShaderReflectionType;
     T_LPD3D11SHADERREFLECTIONVARIABLE       = ^T_ID3D11ShaderReflectionVariable;
     T_LPD3D11SHADERREFLECTIONCONSTANTBUFFER = ^T_ID3D11ShaderReflectionConstantBuffer;
     T_LPD3D11SHADERREFLECTION               = ^T_ID3D11ShaderReflection;
     T_LPD3D11LIBRARYREFLECTION              = ^T_ID3D11LibraryReflection;
     T_LPD3D11FUNCTIONREFLECTION             = ^T_ID3D11FunctionReflection;
     T_LPD3D11FUNCTIONPARAMETERREFLECTION    = ^T_ID3D11FunctionParameterReflection;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ÅyÉCÉìÉ^ÉtÉFÅ[ÉXÅz

     T_ID3D11ShaderReflectionType = interface
       [IID_ID3D11ShaderReflectionType]
       function GetDesc( out pDesc:T_D3D11_SHADER_TYPE_DESC ) :HRESULT; stdcall;
       function GetMemberTypeByIndex( Index:UINT ) :T_ID3D11ShaderReflectionType; stdcall;
       function GetMemberTypeByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionType; stdcall;
       function GetMemberTypeName( Index:UINT ) :LPCSTR; stdcall;
       function IsEqual( pType:T_ID3D11ShaderReflectionType ) :HRESULT; stdcall;
       function GetSubType :T_ID3D11ShaderReflectionType; stdcall;
       function GetBaseClass :T_ID3D11ShaderReflectionType; stdcall;
       function GetNumInterfaces :UINT; stdcall;
       function GetInterfaceByIndex( uIndex:UINT ) :T_ID3D11ShaderReflectionType; stdcall;
       function IsOfType( pType:T_ID3D11ShaderReflectionType ) :HRESULT; stdcall;
       function ImplementsInterface( pBase:T_ID3D11ShaderReflectionType ) :HRESULT; stdcall;
     end;

     T_ID3D11ShaderReflectionVariable = interface
       ['{51F23923-F3E5-4BD1-91CB-606177D8DB4C}']
       function GetDesc( out pDesc:T_D3D11_SHADER_VARIABLE_DESC ) :HRESULT; stdcall;
       function GetType :T_ID3D11ShaderReflectionType; stdcall;
       function GetBuffer :T_ID3D11ShaderReflectionConstantBuffer; stdcall;
       function GetInterfaceSlot( uArrayIndex:UINT ) :UINT; stdcall;
     end;

     T_ID3D11ShaderReflectionConstantBuffer = interface
       ['{EB62D63D-93DD-4318-8AE8-C6F83AD371B8}']
       function GetDesc( pDesc:T_D3D11_SHADER_BUFFER_DESC ) :HRESULT; stdcall;
       function GetVariableByIndex( Index:UINT ) :T_ID3D11ShaderReflectionVariable; stdcall;
       function GetVariableByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionVariable; stdcall;
     end;

     // The ID3D11ShaderReflection IID may change from SDK version to SDK version
     // if the reflection API changes.  This prevents new code with the new API
     // from working with an old binary.  Recompiling with the new header
     // will pick up the new IID.

     T_ID3D11ShaderReflection = interface
       ['{8d536ca1-0cca-4956-a837-786963755584}']
       function GetDesc( out pDesc:T_D3D11_SHADER_DESC ) :HRESULT; stdcall;
       function GetConstantBufferByIndex( Index:UINT ) :T_ID3D11ShaderReflectionConstantBuffer; stdcall;
       function GetConstantBufferByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionConstantBuffer; stdcall;
       function GetResourceBindingDesc( ResourceIndex:UINT; out pDesc:T_D3D11_SHADER_INPUT_BIND_DESC ) :HRESULT; stdcall;
       function GetInputParameterDesc( ParameterIndex:UINT; out pDesc:T_D3D11_SIGNATURE_PARAMETER_DESC ) :HRESULT; stdcall;
       function GetOutputParameterDesc( ParameterIndex:UINT; out pDesc:T_D3D11_SIGNATURE_PARAMETER_DESC ) :HRESULT; stdcall;
       function GetPatchConstantParameterDesc( ParameterIndex:UINT; out pDesc:T_D3D11_SIGNATURE_PARAMETER_DESC ) :HRESULT; stdcall;
       function GetVariableByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionVariable; stdcall;
       function GetResourceBindingDescByName( Name:LPCSTR; out pDesc:T_D3D11_SHADER_INPUT_BIND_DESC ) :HRESULT; stdcall;
       function GetMovInstructionCount :UINT; stdcall;
       function GetMovcInstructionCount :UINT; stdcall;
       function GetConversionInstructionCount :UINT; stdcall;
       function GetBitwiseInstructionCount :UINT; stdcall;
       function GetGSInputPrimitive :T_D3D_PRIMITIVE; stdcall;
       function IsSampleFrequencyShader :BOOL; stdcall;
       function GetNumInterfaceSlots :UINT; stdcall;
       function GetMinFeatureLevel( out pLevel:T_D3D_FEATURE_LEVEL ) :HRESULT; stdcall;
       function GetThreadGroupSize( out pSizeX:UINT; out pSizeY:UINT; out pSizeZ:UINT ) :UINT; stdcall;
       function GetRequiresFlags :UINT64; stdcall;
     end;

     T_ID3D11LibraryReflection = interface
       ['{54384F1B-5B3E-4BB7-AE01-60BA3097CBB6}']
       function GetDesc( out pDesc:T_D3D11_LIBRARY_DESC ) :HRESULT; stdcall;
       function GetFunctionByIndex( FunctionIndex:T_INT ) :T_ID3D11FunctionReflection; stdcall;
     end;

     T_ID3D11FunctionReflection = interface
       ['{207BCECB-D683-4A06-A8A3-9B149B9F73A4}']
       function GetDesc( out pDesc:T_D3D11_FUNCTION_DESC ) :HRESULT; stdcall;
       function GetConstantBufferByIndex( BufferIndex:UINT ) :T_ID3D11ShaderReflectionConstantBuffer; stdcall;
       function GetConstantBufferByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionConstantBuffer; stdcall;

       function GetResourceBindingDesc( ResourceIndex:UINT; out pDesc:T_D3D11_SHADER_INPUT_BIND_DESC ) :HRESULT; stdcall;
       function GetVariableByName( Name:LPCSTR ) :T_ID3D11ShaderReflectionVariable; stdcall;
       function GetResourceBindingDescByName( Name:LPCSTR; out pDesc:T_D3D11_SHADER_INPUT_BIND_DESC ) :HRESULT; stdcall;
       // Use D3D_RETURN_PARAMETER_INDEX to get description of the return value.
       function GetFunctionParameter( ParameterIndex:T_INT ) :T_ID3D11FunctionParameterReflection; stdcall;
     end;

     T_ID3D11FunctionParameterReflection = interface
       ['{42757488-334F-47FE-982E-1A65D08CC462}']
       function GetDesc( out pDesc:T_D3D11_PARAMETER_DESC ) :HRESULT; stdcall;
     end;

     T_ID3D11Module = interface
       ['{CAC701EE-80FC-4122-8242-10B39C8CEC34}']
       // Create an instance of a module for resource re-binding.
       function CreateInstance( pNamespace:LPCSTR; out ppModuleInstance:T_ID3D11ModuleInstance ) :HRESULT; stdcall;
     end;

     T_ID3D11ModuleInstance = interface
       ['{469E07F7-045A-48D5-AA12-68A478CDF75D}']
       //
       // Resource binding API.
       //
       function BindConstantBuffer( uSrcSlot:UINT; uDstSlot:UINT; cbDstOffset:UINT ) :HRESULT; stdcall;
       function BindConstantBufferByName( pName:LPCSTR; uDstSlot:UINT; cbDstOffset:UINT ) :HRESULT; stdcall;

       function BindResource( uSrcSlot:UINT; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;
       function BindResourceByName( pName:LPCSTR; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;

       function BindSampler( uSrcSlot:UINT; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;
       function BindSamplerByName( pName:LPCSTR; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;

       function BindUnorderedAccessView( uSrcSlot:UINT; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;
       function BindUnorderedAccessViewByName( pName:LPCSTR; uDstSlot:UINT; uCount:UINT ) :HRESULT; stdcall;

       function BindResourceAsUnorderedAccessView( uSrcSrvSlot:UINT; uDstUavSlot:UINT; uCount:UINT ) :HRESULT; stdcall;
       function BindResourceAsUnorderedAccessViewByName( pSrvName:LPCSTR; uDstUavSlot:UINT; uCount:UINT ) :HRESULT; stdcall;
     end;

     T_ID3D11Linker = interface
       ['{59A6CD0E-E10D-4C1F-88C0-63ABA1DAF30E}']
       // Link the shader and produce a shader blob suitable to D3D runtime.
       function Link(      pEntry       :T_ID3D11ModuleInstance;
                           pEntryName   :LPCSTR;
                           pTargetName  :LPCSTR;
                           uFlags       :UINT;
                      out ppShaderBlob  :T_ID3DBlob;
                      out ppErrorBuffer :T_ID3DBlob             ) :HRESULT; stdcall;
       // Add an instance of a library module to be used for linking.
       function UseLibrary( pLibraryMI:T_ID3D11ModuleInstance ) :HRESULT; stdcall;
       // Add a clip plane with the plane coefficients taken from a cbuffer entry for 10L9 shaders.
       function AddClipPlaneFromCBuffer( uCBufferSlot:UINT; uCBufferEntry:UINT ) :HRESULT; stdcall;
     end;

     T_ID3D11LinkingNode = interface
       ['{D80DD70C-8D2F-4751-94A1-03C79B3556DB}']
     end;

     T_ID3D11FunctionLinkingGraph = interface
       ['{54133220-1CE8-43D3-8236-9855C5CEECFF}']
       // Create a shader module out of FLG description.
       function CreateModuleInstance( out ppModuleInstance :T_ID3D11ModuleInstance;
                                      out ppErrorBuffer    :T_ID3DBlob             ) :HRESULT; stdcall;
       function SetInputSignature( const pInputParameters :P_D3D11_PARAMETER_DESC;
                                         cInputParameters :UINT;
                                    out ppInputNode       :T_ID3D11LinkingNode    ) :HRESULT; stdcall;
       function SetOutputSignature( const pOutputParameters :P_D3D11_PARAMETER_DESC;
                                          cOutputParameters :UINT;
                                     out ppOutputNode       :T_ID3D11LinkingNode    ) :HRESULT; stdcall;
       function CallFunction(      pModuleInstanceNamespace     :LPCSTR;
                                   pModuleWithFunctionPrototype :T_ID3D11Module;
                                   pFunctionName                :LPCSTR;
                              out ppCallNode                    :T_ID3D11LinkingNode ) :HRESULT; stdcall;
       function PassValue( pSrcNode           :T_ID3D11LinkingNode;
                            SrcParameterIndex :T_INT;
                           pDstNode           :T_ID3D11LinkingNode;
                            DstParameterIndex :T_INT               ) :HRESULT; stdcall;
       function PassValueWithSwizzle( pSrcNode           :T_ID3D11LinkingNode;
                                       SrcParameterIndex :T_INT;
                                      pSrcSwizzle        :LPCSTR;
                                      pDstNode           :T_ID3D11LinkingNode;
                                       DstParameterIndex :T_INT;
                                      pDstSwizzle        :LPCSTR              ) :HRESULT; stdcall;
       function GetLastError( out ppErrorBuffer:T_ID3DBlob ) :HRESULT; stdcall;
       function GenerateHlsl(      uFlags  :UINT;
                              out ppBuffer :T_ID3DBlob ) :HRESULT; stdcall;
     end;

//////////////////////////////////////////////////////////////////////////////
// APIs //////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

implementation //############################################################### Å°

end. //######################################################################### Å°
