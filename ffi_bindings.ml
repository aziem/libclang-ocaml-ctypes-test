open Ctypes

type cx_error_code = 
  | CXError_Success
  | CXError_Failure
  | CXError_Crashed
  | CXError_InvalidArguments
  | CXError_ASTReadError

type kind =
  | CXTypeInvalid
  | CXTypeUnexposed
  | CXTypeVoid
  | CXTypeBool
  | CXTypeChar_U
  | CXTypeUChar
  | CXTypeChar16
  | CXTypeChar32
  | CXTypeUShort
  | CXTypeUInt
  | CXTypeULong
  | CXTypeULongLong
  | CXTypeUInt128
  | CXTypeChar_S
  | CXTypeSChar
  | CXTypeWChar
  | CXTypeShort
  | CXTypeInt
  | CXTypeLong
  | CXTypeLongLong
  | CXTypeInt128
  | CXTypeFloat
  | CXTypeDouble
  | CXTypeLongDouble
  | CXTypeNullPtr
  | CXTypeOverload
  | CXTypeDependent
  | CXTypeObjCId
  | CXTypeObjCClass
  | CXTypeObjCSel
  | CXTypeFirstBuiltin
  | CXTypeLastBuiltin
  | CXTypeComplex
  | CXTypePointer
  | CXTypeBlockPointer
  | CXTypeLValueReference
  | CXTypeRValueReference
  | CXTypeRecord
  | CXTypeEnum
  | CXTypeTypedef
  | CXTypeObjCInterface
  | CXTypeObjCObjectPointer
  | CXTypeFunctionNoProto
  | CXTypeFunctionProto
  | CXTypeConstantArray
  | CXTypeVector
  | CXTypeIncompleteArray
  | CXTypeVariableArray
  | CXTypeDependentSizedArray
  | CXTypeMemberPointer
  | CXTypeAuto
  | CXTypeElaborated

type calling_conv =
  | CXCallingConv_Default
  | CXCallingConv_C
  | CXCallingConv_X86StdCall
  | CXCallingConv_X86FastCall
  | CXCallingConv_X86ThisCall
  | CXCallingConv_X86Pascal
  | CXCallingConv_AAPCS
  | CXCallingConv_AAPCS_VFP
  | CXCallingConv_IntelOclBicc
  | CXCallingConv_X86_64Win64
  | CXCallingConv_X86_64SysV
  | CXCallingConv_Invalid
  | CXCallingConv_Unexposed

type layout_error =
  | CXTypeLayoutError_Invalid
  | CXTypeLayoutError_Incomplete
  | CXTypeLayoutError_Dependent
  | CXTypeLayoutError_NotConstantSize
  | CXTypeLayoutError_InvalidFieldName

type translation_unit_options =
  | CXTranslationUnit_NoOptions
  | CXTranslationUnit_DetailedPreprocessingRecord
  | CXTranslationUnit_Incomplete
  | CXTranslationUnit_PrecompiledPreamble
  | CXTranslationUnit_CacheCompletionResults
  | CXTranslationUnit_ForSerialization
  | CXTranslationUnit_CXXChainedPCH
  | CXTranslationUnit_SkipFunctionBodies
  | CXTranslationUnit_IncludeBriefCommentsInCodeCompletion
  | CXTranslationUnit_CreatePreambleOnFirstParse
  | CXTranslationUnit_KeepGoing

type cursor_kind = 
  | CXCursor_UnexposedDecl                 
  | CXCursor_StructDecl                    
  | CXCursor_UnionDecl                     
  | CXCursor_ClassDecl                     
  | CXCursor_EnumDecl                      
  | CXCursor_FieldDecl                     
  | CXCursor_EnumConstantDecl              
  | CXCursor_FunctionDecl                  
  | CXCursor_VarDecl                       
  | CXCursor_ParmDecl                      
  | CXCursor_ObjCInterfaceDecl             
  | CXCursor_ObjCCategoryDecl              
  | CXCursor_ObjCProtocolDecl              
  | CXCursor_ObjCPropertyDecl              
  | CXCursor_ObjCIvarDecl                  
  | CXCursor_ObjCInstanceMethodDecl        
  | CXCursor_ObjCClassMethodDecl           
  | CXCursor_ObjCImplementationDecl        
  | CXCursor_ObjCCategoryImplDecl          
  | CXCursor_TypedefDecl                   
  | CXCursor_CXXMethod                     
  | CXCursor_Namespace                     
  | CXCursor_LinkageSpec                   
  | CXCursor_Constructor                   
  | CXCursor_Destructor                    
  | CXCursor_ConversionFunction            
  | CXCursor_TemplateTypeParameter         
  | CXCursor_NonTypeTemplateParameter      
  | CXCursor_TemplateTemplateParameter     
  | CXCursor_FunctionTemplate              
  | CXCursor_ClassTemplate                 
  | CXCursor_ClassTemplatePartialSpecialization 
  | CXCursor_NamespaceAlias                
  | CXCursor_UsingDirective                
  | CXCursor_UsingDeclaration              
  | CXCursor_TypeAliasDecl                 
  | CXCursor_ObjCSynthesizeDecl            
  | CXCursor_ObjCDynamicDecl               
  | CXCursor_CXXAccessSpecifier            
  | CXCursor_FirstDecl                     
  | CXCursor_LastDecl                      
  | CXCursor_FirstRef                      
  | CXCursor_ObjCSuperClassRef             
  | CXCursor_ObjCProtocolRef               
  | CXCursor_ObjCClassRef                  
  | CXCursor_TypeRef                       
  | CXCursor_CXXBaseSpecifier              
  | CXCursor_TemplateRef                   
  | CXCursor_NamespaceRef                  
  | CXCursor_MemberRef                     
  | CXCursor_LabelRef                      
  | CXCursor_OverloadedDeclRef             
  | CXCursor_VariableRef                   
  | CXCursor_LastRef                       
  | CXCursor_FirstInvalid                  
  | CXCursor_InvalidFile                   
  | CXCursor_NoDeclFound                   
  | CXCursor_NotImplemented                
  | CXCursor_InvalidCode                   
  | CXCursor_LastInvalid                   
  | CXCursor_FirstExpr                     
  | CXCursor_UnexposedExpr                 
  | CXCursor_DeclRefExpr                   
  | CXCursor_MemberRefExpr                 
  | CXCursor_CallExpr                      
  | CXCursor_ObjCMessageExpr               
  | CXCursor_BlockExpr                     
  | CXCursor_IntegerLiteral                
  | CXCursor_FloatingLiteral               
  | CXCursor_ImaginaryLiteral              
  | CXCursor_StringLiteral                 
  | CXCursor_CharacterLiteral              
  | CXCursor_ParenExpr                     
  | CXCursor_UnaryOperator                 
  | CXCursor_ArraySubscriptExpr            
  | CXCursor_BinaryOperator                
  | CXCursor_CompoundAssignOperator        
  | CXCursor_ConditionalOperator           
  | CXCursor_CStyleCastExpr                
  | CXCursor_CompoundLiteralExpr           
  | CXCursor_InitListExpr                  
  | CXCursor_AddrLabelExpr                 
  | CXCursor_StmtExpr                      
  | CXCursor_GenericSelectionExpr          
  | CXCursor_GNUNullExpr                   
  | CXCursor_CXXStaticCastExpr             
  | CXCursor_CXXDynamicCastExpr            
  | CXCursor_CXXReinterpretCastExpr        
  | CXCursor_CXXConstCastExpr              
  | CXCursor_CXXFunctionalCastExpr         
  | CXCursor_CXXTypeidExpr                 
  | CXCursor_CXXBoolLiteralExpr            
  | CXCursor_CXXNullPtrLiteralExpr         
  | CXCursor_CXXThisExpr                   
  | CXCursor_CXXThrowExpr                  
  | CXCursor_CXXNewExpr                    
  | CXCursor_CXXDeleteExpr                 
  | CXCursor_UnaryExpr                     
  | CXCursor_ObjCStringLiteral             
  | CXCursor_ObjCEncodeExpr                
  | CXCursor_ObjCSelectorExpr              
  | CXCursor_ObjCProtocolExpr              
  | CXCursor_ObjCBridgedCastExpr           
  | CXCursor_PackExpansionExpr             
  | CXCursor_SizeOfPackExpr                
  | CXCursor_LambdaExpr                    
  | CXCursor_ObjCBoolLiteralExpr           
  | CXCursor_ObjCSelfExpr                  
  | CXCursor_OMPArraySectionExpr           
  | CXCursor_ObjCAvailabilityCheckExpr     
  | CXCursor_LastExpr                      
  | CXCursor_FirstStmt                     
  | CXCursor_UnexposedStmt                 
  | CXCursor_LabelStmt                     
  | CXCursor_CompoundStmt                  
  | CXCursor_CaseStmt                      
  | CXCursor_DefaultStmt                   
  | CXCursor_IfStmt                        
  | CXCursor_SwitchStmt                    
  | CXCursor_WhileStmt                     
  | CXCursor_DoStmt                        
  | CXCursor_ForStmt                       
  | CXCursor_GotoStmt                      
  | CXCursor_IndirectGotoStmt              
  | CXCursor_ContinueStmt                  
  | CXCursor_BreakStmt                     
  | CXCursor_ReturnStmt                    
  | CXCursor_GCCAsmStmt                    
  | CXCursor_AsmStmt                       
  | CXCursor_ObjCAtTryStmt                 
  | CXCursor_ObjCAtCatchStmt               
  | CXCursor_ObjCAtFinallyStmt             
  | CXCursor_ObjCAtThrowStmt               
  | CXCursor_ObjCAtSynchronizedStmt        
  | CXCursor_ObjCAutoreleasePoolStmt       
  | CXCursor_ObjCForCollectionStmt         
  | CXCursor_CXXCatchStmt                  
  | CXCursor_CXXTryStmt                    
  | CXCursor_CXXForRangeStmt               
  | CXCursor_SEHTryStmt                    
  | CXCursor_SEHExceptStmt                 
  | CXCursor_SEHFinallyStmt                
  | CXCursor_MSAsmStmt                     
  | CXCursor_NullStmt                      
  | CXCursor_DeclStmt                      
  | CXCursor_OMPParallelDirective          
  | CXCursor_OMPSimdDirective              
  | CXCursor_OMPForDirective               
  | CXCursor_OMPSectionsDirective          
  | CXCursor_OMPSectionDirective           
  | CXCursor_OMPSingleDirective            
  | CXCursor_OMPParallelForDirective       
  | CXCursor_OMPParallelSectionsDirective  
  | CXCursor_OMPTaskDirective              
  | CXCursor_OMPMasterDirective            
  | CXCursor_OMPCriticalDirective          
  | CXCursor_OMPTaskyieldDirective         
  | CXCursor_OMPBarrierDirective           
  | CXCursor_OMPTaskwaitDirective          
  | CXCursor_OMPFlushDirective             
  | CXCursor_SEHLeaveStmt                  
  | CXCursor_OMPOrderedDirective           
  | CXCursor_OMPAtomicDirective            
  | CXCursor_OMPForSimdDirective           
  | CXCursor_OMPParallelForSimdDirective   
  | CXCursor_OMPTargetDirective            
  | CXCursor_OMPTeamsDirective             
  | CXCursor_OMPTaskgroupDirective         
  | CXCursor_OMPCancellationPointDirective 
  | CXCursor_OMPCancelDirective            
  | CXCursor_OMPTargetDataDirective        
  | CXCursor_OMPTaskLoopDirective          
  | CXCursor_OMPTaskLoopSimdDirective      
  | CXCursor_OMPDistributeDirective        
  | CXCursor_OMPTargetEnterDataDirective   
  | CXCursor_OMPTargetExitDataDirective    
  | CXCursor_OMPTargetParallelDirective    
  | CXCursor_OMPTargetParallelForDirective 
  | CXCursor_OMPTargetUpdateDirective      
  | CXCursor_OMPDistributeParallelForDirective 
  | CXCursor_OMPDistributeParallelForSimdDirective 
  | CXCursor_OMPDistributeSimdDirective 
  | CXCursor_OMPTargetParallelForSimdDirective 
  | CXCursor_LastStmt 
  | CXCursor_TranslationUnit               
  | CXCursor_FirstAttr                     
  | CXCursor_UnexposedAttr                 
  | CXCursor_IBActionAttr                  
  | CXCursor_IBOutletAttr                  
  | CXCursor_IBOutletCollectionAttr        
  | CXCursor_CXXFinalAttr                  
  | CXCursor_CXXOverrideAttr               
  | CXCursor_AnnotateAttr                  
  | CXCursor_AsmLabelAttr                  
  | CXCursor_PackedAttr                    
  | CXCursor_PureAttr                      
  | CXCursor_ConstAttr                     
  | CXCursor_NoDuplicateAttr               
  | CXCursor_CUDAConstantAttr              
  | CXCursor_CUDADeviceAttr                
  | CXCursor_CUDAGlobalAttr                
  | CXCursor_CUDAHostAttr                  
  | CXCursor_CUDASharedAttr                
  | CXCursor_VisibilityAttr                
  | CXCursor_DLLExport                     
  | CXCursor_DLLImport                     
  | CXCursor_LastAttr                      
  | CXCursor_PreprocessingDirective        
  | CXCursor_MacroDefinition               
  | CXCursor_MacroExpansion                
  | CXCursor_MacroInstantiation            
  | CXCursor_InclusionDirective            
  | CXCursor_FirstPreprocessing            
  | CXCursor_LastPreprocessing             
  | CXCursor_ModuleImportDecl              
  | CXCursor_TypeAliasTemplateDecl         
  | CXCursor_StaticAssert                  
  | CXCursor_FirstExtraDecl                
  | CXCursor_LastExtraDecl                 
  | CXCursor_OverloadCandidate             

type cx_child_visit_result =
  | CXChildVisitBreak
  | CXChildVisitContinue
  | CXChildVisitRecurse

type cx_string
type cx_type
type cx_cursor

type cx_client_data = unit Ctypes.ptr
type cx_translation_unit = unit Ctypes.ptr
type cx_index = unit Ctypes.ptr

module Enums (T : Cstubs_structs.TYPE) =
struct
  let cx_string : cx_string Ctypes.structure T.typ = T.structure "_CXString"
  let cx_string = T.typedef cx_string "CXString"
  let cx_string_data = T.field cx_string "data" (T.ptr T.void) 
  let cx_string_private_flags = T.field cx_string "private_flags" T.uint 
  let () = T.seal cx_string 


  let cxerror_success = T.constant "CXError_Success" T.int64_t
  let cxerror_failure = T.constant "CXError_Failure" T.int64_t
  let cxerror_crashed = T.constant "CXError_Crashed" T.int64_t
  let cxerror_invalidarguments = T.constant "CXError_InvalidArguments" T.int64_t
  let cxerror_astreaderror = T.constant "CXError_ASTReadError" T.int64_t

  let cx_error_code = T.enum "CXErrorCode" [
      CXError_Success, cxerror_success;
      CXError_Failure, cxerror_failure;
      CXError_Crashed, cxerror_crashed;
      CXError_InvalidArguments, cxerror_invalidarguments;
      CXError_ASTReadError, cxerror_astreaderror
    ]

  let invalid             = T.constant "CXType_Invalid" T.int64_t
  let unexposed           = T.constant "CXType_Unexposed" T.int64_t
  let cxvoid              = T.constant "CXType_Void" T.int64_t
  let bool                = T.constant "CXType_Bool" T.int64_t
  let char_u              = T.constant "CXType_Char_U" T.int64_t
  let uchar               = T.constant "CXType_UChar" T.int64_t
  let char16              = T.constant "CXType_Char16" T.int64_t
  let char32              = T.constant "CXType_Char32" T.int64_t
  let ushort              = T.constant "CXType_UShort" T.int64_t
  let uint                = T.constant "CXType_UInt" T.int64_t
  let ulong               = T.constant "CXType_ULong" T.int64_t
  let ulonglong           = T.constant "CXType_ULongLong" T.int64_t
  let uint128             = T.constant "CXType_UInt128" T.int64_t
  let char_s              = T.constant "CXType_Char_S" T.int64_t
  let schar               = T.constant "CXType_SChar" T.int64_t
  let wchar               = T.constant "CXType_WChar" T.int64_t
  let short               = T.constant "CXType_Short" T.int64_t
  let int                 = T.constant "CXType_Int" T.int64_t
  let long                = T.constant "CXType_Long" T.int64_t
  let longlong            = T.constant "CXType_LongLong" T.int64_t
  let int128              = T.constant "CXType_Int128" T.int64_t
  let float               = T.constant "CXType_Float" T.int64_t
  let double              = T.constant "CXType_Double" T.int64_t
  let longdouble          = T.constant "CXType_LongDouble" T.int64_t
  let nullptr             = T.constant "CXType_NullPtr" T.int64_t
  let overload            = T.constant "CXType_Overload" T.int64_t
  let dependent           = T.constant "CXType_Dependent" T.int64_t
  let objcid              = T.constant "CXType_ObjCId" T.int64_t
  let objcclass           = T.constant "CXType_ObjCClass" T.int64_t
  let objcsel             = T.constant "CXType_ObjCSel" T.int64_t
  let float128            = T.constant "CXType_Float128" T.int64_t
  let firstbuiltin        = T.constant "CXType_FirstBuiltin" T.int64_t
  let lastbuiltin         = T.constant "CXType_LastBuiltin"  T.int64_t
  let complex             = T.constant "CXType_Complex" T.int64_t
  let pointer             = T.constant "CXType_Pointer" T.int64_t
  let blockpointer        = T.constant "CXType_BlockPointer" T.int64_t
  let lvaluereference     = T.constant "CXType_LValueReference" T.int64_t
  let rvaluereference     = T.constant "CXType_RValueReference" T.int64_t
  let record              = T.constant "CXType_Record" T.int64_t
  let cxenum              = T.constant "CXType_Enum" T.int64_t
  let typedef             = T.constant "CXType_Typedef" T.int64_t
  let objcinterface       = T.constant "CXType_ObjCInterface" T.int64_t
  let objcobjectpointer   = T.constant "CXType_ObjCObjectPointer" T.int64_t
  let functionnoproto     = T.constant "CXType_FunctionNoProto" T.int64_t
  let functionproto       = T.constant "CXType_FunctionProto" T.int64_t
  let constantarray       = T.constant "CXType_ConstantArray" T.int64_t
  let vector              = T.constant "CXType_Vector" T.int64_t
  let incompletearray     = T.constant "CXType_IncompleteArray" T.int64_t
  let variablearray       = T.constant "CXType_VariableArray" T.int64_t
  let dependentsizedarray = T.constant "CXType_DependentSizedArray" T.int64_t
  let memberpointer       = T.constant "CXType_MemberPointer" T.int64_t
  let auto                = T.constant "CXType_Auto" T.int64_t
  let elaborated          = T.constant "CXType_Elaborated" T.int64_t

  let kind = T.enum "CXTypeKind" [
      CXTypeInvalid, invalid;
      CXTypeUnexposed, unexposed;
      CXTypeVoid, cxvoid;
      CXTypeBool, bool;
      CXTypeChar_U, char_u;
      CXTypeUChar, uchar;
      CXTypeChar16, char16;
      CXTypeChar32, char32;
      CXTypeUShort, ushort;
      CXTypeUInt, uint;
      CXTypeULong, ulong;
      CXTypeULongLong, ulonglong;
      CXTypeUInt128, uint128;
      CXTypeChar_S, char_s;
      CXTypeSChar, schar;
      CXTypeWChar, wchar;
      CXTypeShort, short;
      CXTypeInt, int;
      CXTypeLong, long;
      CXTypeLongLong, longlong;
      CXTypeInt128, int128;
      CXTypeFloat, float;
      CXTypeDouble, double;
      CXTypeLongDouble, longdouble;
      CXTypeNullPtr, nullptr;
      CXTypeOverload, overload;
      CXTypeDependent, dependent;
      CXTypeObjCId, objcid;
      CXTypeObjCClass, objcclass;
      CXTypeObjCSel, objcsel;
      CXTypeFirstBuiltin, firstbuiltin;
      CXTypeLastBuiltin, lastbuiltin;
      CXTypeComplex, complex;
      CXTypePointer, pointer;
      CXTypeBlockPointer, blockpointer;
      CXTypeLValueReference, lvaluereference;
      CXTypeRValueReference, rvaluereference;
      CXTypeRecord, record;
      CXTypeEnum, cxenum;
      CXTypeTypedef, typedef;
      CXTypeObjCInterface, objcinterface;
      CXTypeObjCObjectPointer, objcobjectpointer;
      CXTypeFunctionNoProto, functionnoproto;
      CXTypeFunctionProto, functionproto;
      CXTypeConstantArray, constantarray;
      CXTypeVector, vector;
      CXTypeIncompleteArray, incompletearray;
      CXTypeVariableArray, variablearray;
      CXTypeDependentSizedArray, dependentsizedarray;
      CXTypeMemberPointer, memberpointer;
      CXTypeAuto, auto;
      CXTypeElaborated, elaborated;
    ]

  let cxcallingconv_default       = T.constant "CXCallingConv_Default" T.int64_t
  let cxcallingconv_c             = T.constant "CXCallingConv_C" T.int64_t
  let cxcallingconv_x86stdcall    = T.constant "CXCallingConv_X86StdCall" T.int64_t
  let cxcallingconv_x86fastcall   = T.constant "CXCallingConv_X86FastCall" T.int64_t
  let cxcallingconv_x86thiscall   = T.constant "CXCallingConv_X86ThisCall" T.int64_t
  let cxcallingconv_x86pascal     = T.constant "CXCallingConv_X86Pascal" T.int64_t
  let cxcallingconv_aapcs         = T.constant "CXCallingConv_AAPCS" T.int64_t
  let cxcallingconv_aapcs_vfp     = T.constant "CXCallingConv_AAPCS_VFP" T.int64_t
  let cxcallingconv_inteloclbicc  = T.constant "CXCallingConv_IntelOclBicc" T.int64_t
  let cxcallingconv_x86_64win64   = T.constant "CXCallingConv_X86_64Win64" T.int64_t
  let cxcallingconv_x86_64sysv    = T.constant "CXCallingConv_X86_64SysV" T.int64_t
  let cxcallingconv_x86vectorcall = T.constant "CXCallingConv_X86VectorCall" T.int64_t
  let cxcallingconv_swift         = T.constant "CXCallingConv_Swift" T.int64_t
  let cxcallingconv_preservemost  = T.constant "CXCallingConv_PreserveMost" T.int64_t
  let cxcallingconv_preserveall   = T.constant "CXCallingConv_PreserveAll" T.int64_t
  let cxcallingconv_invalid       = T.constant "CXCallingConv_Invalid" T.int64_t
  let cxcallingconv_unexposed     = T.constant "CXCallingConv_Unexposed" T.int64_t

  let calling_conv = T.enum "CXCallingConv" [
      CXCallingConv_Default, cxcallingconv_default;
      CXCallingConv_C, cxcallingconv_c;
      CXCallingConv_X86StdCall, cxcallingconv_x86stdcall;
      CXCallingConv_X86FastCall, cxcallingconv_x86fastcall;
      CXCallingConv_X86ThisCall, cxcallingconv_x86thiscall;
      CXCallingConv_X86Pascal, cxcallingconv_x86pascal;
      CXCallingConv_AAPCS, cxcallingconv_aapcs;
      CXCallingConv_AAPCS_VFP, cxcallingconv_aapcs_vfp;
      CXCallingConv_IntelOclBicc, cxcallingconv_inteloclbicc;
      CXCallingConv_X86_64Win64, cxcallingconv_x86_64win64;
      CXCallingConv_X86_64SysV, cxcallingconv_x86_64sysv;
      CXCallingConv_Invalid, cxcallingconv_invalid;
      CXCallingConv_Unexposed, cxcallingconv_unexposed;
    ]

  let invalidlayout          = T.constant "CXTypeLayoutError_Invalid"  T.int64_t
  let incompletelayout       = T.constant "CXTypeLayoutError_Incomplete"  T.int64_t
  let dependentlayout        = T.constant "CXTypeLayoutError_Dependent"  T.int64_t
  let notconstantsizelayout  = T.constant "CXTypeLayoutError_NotConstantSize"  T.int64_t
  let invalidfieldnamelayout = T.constant "CXTypeLayoutError_InvalidFieldName"  T.int64_t

  let layout_error = T.enum "CXTypeLayoutError" [
      CXTypeLayoutError_Invalid, invalidlayout;
      CXTypeLayoutError_Incomplete, incompletelayout;
      CXTypeLayoutError_Dependent, dependentlayout;
      CXTypeLayoutError_NotConstantSize, notconstantsizelayout;
      CXTypeLayoutError_InvalidFieldName, invalidfieldnamelayout;
    ]

  let nooptions                            = T.constant "CXTranslationUnit_None" T.int64_t
  let detailedpreprocessingrecord          = T.constant "CXTranslationUnit_DetailedPreprocessingRecord" T.int64_t
  let incomplete                           = T.constant "CXTranslationUnit_Incomplete" T.int64_t
  let precompiledpreamble                  = T.constant "CXTranslationUnit_PrecompiledPreamble" T.int64_t
  let cachecompletionresults               = T.constant "CXTranslationUnit_CacheCompletionResults" T.int64_t
  let forserialization                     = T.constant "CXTranslationUnit_ForSerialization" T.int64_t
  let cxxchainedpch                        = T.constant "CXTranslationUnit_CXXChainedPCH" T.int64_t
  let skipfunctionbodies                   = T.constant "CXTranslationUnit_SkipFunctionBodies" T.int64_t
  let includebriefcommentsincodecompletion = T.constant "CXTranslationUnit_IncludeBriefCommentsInCodeCompletion" T.int64_t
  let createpreambleonfirstparse           = T.constant "CXTranslationUnit_CreatePreambleOnFirstParse" T.int64_t
  let keepgoing                            = T.constant "CXTranslationUnit_KeepGoing" T.int64_t

  let translation_unit_options = T.enum "CXTranslationUnit_Flags"
      [
        CXTranslationUnit_NoOptions, nooptions;
        CXTranslationUnit_DetailedPreprocessingRecord, detailedpreprocessingrecord;
        CXTranslationUnit_Incomplete, incomplete;
        CXTranslationUnit_PrecompiledPreamble, precompiledpreamble;
        CXTranslationUnit_CacheCompletionResults, cachecompletionresults;
        CXTranslationUnit_ForSerialization, forserialization;
        CXTranslationUnit_CXXChainedPCH, cxxchainedpch;
        CXTranslationUnit_SkipFunctionBodies, skipfunctionbodies;
        CXTranslationUnit_IncludeBriefCommentsInCodeCompletion, includebriefcommentsincodecompletion;
        CXTranslationUnit_CreatePreambleOnFirstParse, createpreambleonfirstparse;
        CXTranslationUnit_KeepGoing, keepgoing;
      ]

  let unexposeddecl                         = T.constant "CXCursor_UnexposedDecl" T.int64_t                 
  let structdecl                            = T.constant "CXCursor_StructDecl" T.int64_t                    
  let uniondecl                             = T.constant "CXCursor_UnionDecl" T.int64_t                     
  let classdecl                             = T.constant "CXCursor_ClassDecl" T.int64_t                     
  let enumdecl                              = T.constant "CXCursor_EnumDecl" T.int64_t                      
  let fielddecl                             = T.constant "CXCursor_FieldDecl" T.int64_t                     
  let enumconstantdecl                      = T.constant "CXCursor_EnumConstantDecl" T.int64_t              
  let functiondecl                          = T.constant "CXCursor_FunctionDecl" T.int64_t                  
  let vardecl                               = T.constant "CXCursor_VarDecl" T.int64_t                       
  let parmdecl                              = T.constant "CXCursor_ParmDecl" T.int64_t                      
  let objcinterfacedecl                     = T.constant "CXCursor_ObjCInterfaceDecl" T.int64_t             
  let objccategorydecl                      = T.constant "CXCursor_ObjCCategoryDecl" T.int64_t              
  let objcprotocoldecl                      = T.constant "CXCursor_ObjCProtocolDecl" T.int64_t              
  let objcpropertydecl                      = T.constant "CXCursor_ObjCPropertyDecl" T.int64_t              
  let objcivardecl                          = T.constant "CXCursor_ObjCIvarDecl" T.int64_t                  
  let objcinstancemethoddecl                = T.constant "CXCursor_ObjCInstanceMethodDecl" T.int64_t        
  let objcclassmethoddecl                   = T.constant "CXCursor_ObjCClassMethodDecl" T.int64_t           
  let objcimplementationdecl                = T.constant "CXCursor_ObjCImplementationDecl" T.int64_t        
  let objccategoryimpldecl                  = T.constant "CXCursor_ObjCCategoryImplDecl" T.int64_t          
  let typedefdecl                           = T.constant "CXCursor_TypedefDecl" T.int64_t                   
  let cxxmethod                             = T.constant "CXCursor_CXXMethod" T.int64_t                     
  let namespace                             = T.constant "CXCursor_Namespace" T.int64_t                     
  let linkagespec                           = T.constant "CXCursor_LinkageSpec" T.int64_t                   
  let constructor                           = T.constant "CXCursor_Constructor" T.int64_t                   
  let destructor                            = T.constant "CXCursor_Destructor" T.int64_t                    
  let conversionfunction                    = T.constant "CXCursor_ConversionFunction" T.int64_t            
  let templatetypeparameter                 = T.constant "CXCursor_TemplateTypeParameter" T.int64_t         
  let nontypetemplateparameter              = T.constant "CXCursor_NonTypeTemplateParameter" T.int64_t      
  let templatetemplateparameter             = T.constant "CXCursor_TemplateTemplateParameter" T.int64_t     
  let functiontemplate                      = T.constant "CXCursor_FunctionTemplate" T.int64_t              
  let classtemplate                         = T.constant "CXCursor_ClassTemplate" T.int64_t                 
  let classtemplatepartialspecialization    = T.constant "CXCursor_ClassTemplatePartialSpecialization" T.int64_t 
  let namespacealias                        = T.constant "CXCursor_NamespaceAlias" T.int64_t                
  let usingdirective                        = T.constant "CXCursor_UsingDirective" T.int64_t                
  let usingdeclaration                      = T.constant "CXCursor_UsingDeclaration" T.int64_t              
  let typealiasdecl                         = T.constant "CXCursor_TypeAliasDecl" T.int64_t                 
  let objcsynthesizedecl                    = T.constant "CXCursor_ObjCSynthesizeDecl" T.int64_t            
  let objcdynamicdecl                       = T.constant "CXCursor_ObjCDynamicDecl" T.int64_t               
  let cxxaccessspecifier                    = T.constant "CXCursor_CXXAccessSpecifier" T.int64_t            
  let firstdecl                             = T.constant "CXCursor_FirstDecl" T.int64_t                     
  let lastdecl                              = T.constant "CXCursor_LastDecl" T.int64_t                      
  let firstref                              = T.constant "CXCursor_FirstRef" T.int64_t                      
  let objcsuperclassref                     = T.constant "CXCursor_ObjCSuperClassRef" T.int64_t             
  let objcprotocolref                       = T.constant "CXCursor_ObjCProtocolRef" T.int64_t               
  let objcclassref                          = T.constant "CXCursor_ObjCClassRef" T.int64_t                  
  let typeref                               = T.constant "CXCursor_TypeRef" T.int64_t                       
  let cxxbasespecifier                      = T.constant "CXCursor_CXXBaseSpecifier" T.int64_t              
  let templateref                           = T.constant "CXCursor_TemplateRef" T.int64_t                   
  let namespaceref                          = T.constant "CXCursor_NamespaceRef" T.int64_t                  
  let memberref                             = T.constant "CXCursor_MemberRef" T.int64_t                     
  let labelref                              = T.constant "CXCursor_LabelRef" T.int64_t                      
  let overloadeddeclref                     = T.constant "CXCursor_OverloadedDeclRef" T.int64_t             
  let variableref                           = T.constant "CXCursor_VariableRef" T.int64_t                   
  let lastref                               = T.constant "CXCursor_LastRef" T.int64_t                       
  let firstinvalid                          = T.constant "CXCursor_FirstInvalid" T.int64_t                  
  let invalidfile                           = T.constant "CXCursor_InvalidFile" T.int64_t                   
  let nodeclfound                           = T.constant "CXCursor_NoDeclFound" T.int64_t                   
  let notimplemented                        = T.constant "CXCursor_NotImplemented" T.int64_t                
  let invalidcode                           = T.constant "CXCursor_InvalidCode" T.int64_t                   
  let lastinvalid                           = T.constant "CXCursor_LastInvalid" T.int64_t                   
  let firstexpr                             = T.constant "CXCursor_FirstExpr" T.int64_t                     
  let unexposedexpr                         = T.constant "CXCursor_UnexposedExpr" T.int64_t                 
  let declrefexpr                           = T.constant "CXCursor_DeclRefExpr" T.int64_t                   
  let memberrefexpr                         = T.constant "CXCursor_MemberRefExpr" T.int64_t                 
  let callexpr                              = T.constant "CXCursor_CallExpr" T.int64_t                      
  let objcmessageexpr                       = T.constant "CXCursor_ObjCMessageExpr" T.int64_t               
  let blockexpr                             = T.constant "CXCursor_BlockExpr" T.int64_t                     
  let integerliteral                        = T.constant "CXCursor_IntegerLiteral" T.int64_t                
  let floatingliteral                       = T.constant "CXCursor_FloatingLiteral" T.int64_t               
  let imaginaryliteral                      = T.constant "CXCursor_ImaginaryLiteral" T.int64_t              
  let stringliteral                         = T.constant "CXCursor_StringLiteral" T.int64_t                 
  let characterliteral                      = T.constant "CXCursor_CharacterLiteral" T.int64_t              
  let parenexpr                             = T.constant "CXCursor_ParenExpr" T.int64_t                     
  let unaryoperator                         = T.constant "CXCursor_UnaryOperator" T.int64_t                 
  let arraysubscriptexpr                    = T.constant "CXCursor_ArraySubscriptExpr" T.int64_t            
  let binaryoperator                        = T.constant "CXCursor_BinaryOperator" T.int64_t                
  let compoundassignoperator                = T.constant "CXCursor_CompoundAssignOperator" T.int64_t        
  let conditionaloperator                   = T.constant "CXCursor_ConditionalOperator" T.int64_t           
  let cstylecastexpr                        = T.constant "CXCursor_CStyleCastExpr" T.int64_t                
  let compoundliteralexpr                   = T.constant "CXCursor_CompoundLiteralExpr" T.int64_t           
  let initlistexpr                          = T.constant "CXCursor_InitListExpr" T.int64_t                  
  let addrlabelexpr                         = T.constant "CXCursor_AddrLabelExpr" T.int64_t                 
  let stmtexpr                              = T.constant "CXCursor_StmtExpr" T.int64_t                      
  let genericselectionexpr                  = T.constant "CXCursor_GenericSelectionExpr" T.int64_t          
  let gnunullexpr                           = T.constant "CXCursor_GNUNullExpr" T.int64_t                   
  let cxxstaticcastexpr                     = T.constant "CXCursor_CXXStaticCastExpr" T.int64_t             
  let cxxdynamiccastexpr                    = T.constant "CXCursor_CXXDynamicCastExpr" T.int64_t            
  let cxxreinterpretcastexpr                = T.constant "CXCursor_CXXReinterpretCastExpr" T.int64_t        
  let cxxconstcastexpr                      = T.constant "CXCursor_CXXConstCastExpr" T.int64_t              
  let cxxfunctionalcastexpr                 = T.constant "CXCursor_CXXFunctionalCastExpr" T.int64_t         
  let cxxtypeidexpr                         = T.constant "CXCursor_CXXTypeidExpr" T.int64_t                 
  let cxxboolliteralexpr                    = T.constant "CXCursor_CXXBoolLiteralExpr" T.int64_t            
  let cxxnullptrliteralexpr                 = T.constant "CXCursor_CXXNullPtrLiteralExpr" T.int64_t         
  let cxxthisexpr                           = T.constant "CXCursor_CXXThisExpr" T.int64_t                   
  let cxxthrowexpr                          = T.constant "CXCursor_CXXThrowExpr" T.int64_t                  
  let cxxnewexpr                            = T.constant "CXCursor_CXXNewExpr" T.int64_t                    
  let cxxdeleteexpr                         = T.constant "CXCursor_CXXDeleteExpr" T.int64_t                 
  let unaryexpr                             = T.constant "CXCursor_UnaryExpr" T.int64_t                     
  let objcstringliteral                     = T.constant "CXCursor_ObjCStringLiteral" T.int64_t             
  let objcencodeexpr                        = T.constant "CXCursor_ObjCEncodeExpr" T.int64_t                
  let objcselectorexpr                      = T.constant "CXCursor_ObjCSelectorExpr" T.int64_t              
  let objcprotocolexpr                      = T.constant "CXCursor_ObjCProtocolExpr" T.int64_t              
  let objcbridgedcastexpr                   = T.constant "CXCursor_ObjCBridgedCastExpr" T.int64_t           
  let packexpansionexpr                     = T.constant "CXCursor_PackExpansionExpr" T.int64_t             
  let sizeofpackexpr                        = T.constant "CXCursor_SizeOfPackExpr" T.int64_t                
  let lambdaexpr                            = T.constant "CXCursor_LambdaExpr" T.int64_t                    
  let objcboolliteralexpr                   = T.constant "CXCursor_ObjCBoolLiteralExpr" T.int64_t           
  let objcselfexpr                          = T.constant "CXCursor_ObjCSelfExpr" T.int64_t                  
  let omparraysectionexpr                   = T.constant "CXCursor_OMPArraySectionExpr" T.int64_t           
  let objcavailabilitycheckexpr             = T.constant "CXCursor_ObjCAvailabilityCheckExpr" T.int64_t     
  let lastexpr                              = T.constant "CXCursor_LastExpr" T.int64_t                      
  let firststmt                             = T.constant "CXCursor_FirstStmt" T.int64_t                     
  let unexposedstmt                         = T.constant "CXCursor_UnexposedStmt" T.int64_t                 
  let labelstmt                             = T.constant "CXCursor_LabelStmt" T.int64_t                     
  let compoundstmt                          = T.constant "CXCursor_CompoundStmt" T.int64_t                  
  let casestmt                              = T.constant "CXCursor_CaseStmt" T.int64_t                      
  let defaultstmt                           = T.constant "CXCursor_DefaultStmt" T.int64_t                   
  let ifstmt                                = T.constant "CXCursor_IfStmt" T.int64_t                        
  let switchstmt                            = T.constant "CXCursor_SwitchStmt" T.int64_t                    
  let whilestmt                             = T.constant "CXCursor_WhileStmt" T.int64_t                     
  let dostmt                                = T.constant "CXCursor_DoStmt" T.int64_t                        
  let forstmt                               = T.constant "CXCursor_ForStmt" T.int64_t                       
  let gotostmt                              = T.constant "CXCursor_GotoStmt" T.int64_t                      
  let indirectgotostmt                      = T.constant "CXCursor_IndirectGotoStmt" T.int64_t              
  let continuestmt                          = T.constant "CXCursor_ContinueStmt" T.int64_t                  
  let breakstmt                             = T.constant "CXCursor_BreakStmt" T.int64_t                     
  let returnstmt                            = T.constant "CXCursor_ReturnStmt" T.int64_t                    
  let gccasmstmt                            = T.constant "CXCursor_GCCAsmStmt" T.int64_t                    
  let asmstmt                               = T.constant "CXCursor_AsmStmt" T.int64_t                       
  let objcattrystmt                         = T.constant "CXCursor_ObjCAtTryStmt" T.int64_t                 
  let objcatcatchstmt                       = T.constant "CXCursor_ObjCAtCatchStmt" T.int64_t               
  let objcatfinallystmt                     = T.constant "CXCursor_ObjCAtFinallyStmt" T.int64_t             
  let objcatthrowstmt                       = T.constant "CXCursor_ObjCAtThrowStmt" T.int64_t               
  let objcatsynchronizedstmt                = T.constant "CXCursor_ObjCAtSynchronizedStmt" T.int64_t        
  let objcautoreleasepoolstmt               = T.constant "CXCursor_ObjCAutoreleasePoolStmt" T.int64_t       
  let objcforcollectionstmt                 = T.constant "CXCursor_ObjCForCollectionStmt" T.int64_t         
  let cxxcatchstmt                          = T.constant "CXCursor_CXXCatchStmt" T.int64_t                  
  let cxxtrystmt                            = T.constant "CXCursor_CXXTryStmt" T.int64_t                    
  let cxxforrangestmt                       = T.constant "CXCursor_CXXForRangeStmt" T.int64_t               
  let sehtrystmt                            = T.constant "CXCursor_SEHTryStmt" T.int64_t                    
  let sehexceptstmt                         = T.constant "CXCursor_SEHExceptStmt" T.int64_t                 
  let sehfinallystmt                        = T.constant "CXCursor_SEHFinallyStmt" T.int64_t                
  let msasmstmt                             = T.constant "CXCursor_MSAsmStmt" T.int64_t                     
  let nullstmt                              = T.constant "CXCursor_NullStmt" T.int64_t                      
  let declstmt                              = T.constant "CXCursor_DeclStmt" T.int64_t                      
  let ompparalleldirective                  = T.constant "CXCursor_OMPParallelDirective" T.int64_t          
  let ompsimddirective                      = T.constant "CXCursor_OMPSimdDirective" T.int64_t              
  let ompfordirective                       = T.constant "CXCursor_OMPForDirective" T.int64_t               
  let ompsectionsdirective                  = T.constant "CXCursor_OMPSectionsDirective" T.int64_t          
  let ompsectiondirective                   = T.constant "CXCursor_OMPSectionDirective" T.int64_t           
  let ompsingledirective                    = T.constant "CXCursor_OMPSingleDirective" T.int64_t            
  let ompparallelfordirective               = T.constant "CXCursor_OMPParallelForDirective" T.int64_t       
  let ompparallelsectionsdirective          = T.constant "CXCursor_OMPParallelSectionsDirective" T.int64_t  
  let omptaskdirective                      = T.constant "CXCursor_OMPTaskDirective" T.int64_t              
  let ompmasterdirective                    = T.constant "CXCursor_OMPMasterDirective" T.int64_t            
  let ompcriticaldirective                  = T.constant "CXCursor_OMPCriticalDirective" T.int64_t          
  let omptaskyielddirective                 = T.constant "CXCursor_OMPTaskyieldDirective" T.int64_t         
  let ompbarrierdirective                   = T.constant "CXCursor_OMPBarrierDirective" T.int64_t           
  let omptaskwaitdirective                  = T.constant "CXCursor_OMPTaskwaitDirective" T.int64_t          
  let ompflushdirective                     = T.constant "CXCursor_OMPFlushDirective" T.int64_t             
  let sehleavestmt                          = T.constant "CXCursor_SEHLeaveStmt" T.int64_t                  
  let ompordereddirective                   = T.constant "CXCursor_OMPOrderedDirective" T.int64_t           
  let ompatomicdirective                    = T.constant "CXCursor_OMPAtomicDirective" T.int64_t            
  let ompforsimddirective                   = T.constant "CXCursor_OMPForSimdDirective" T.int64_t           
  let ompparallelforsimddirective           = T.constant "CXCursor_OMPParallelForSimdDirective" T.int64_t   
  let omptargetdirective                    = T.constant "CXCursor_OMPTargetDirective" T.int64_t            
  let ompteamsdirective                     = T.constant "CXCursor_OMPTeamsDirective" T.int64_t             
  let omptaskgroupdirective                 = T.constant "CXCursor_OMPTaskgroupDirective" T.int64_t         
  let ompcancellationpointdirective         = T.constant "CXCursor_OMPCancellationPointDirective" T.int64_t 
  let ompcanceldirective                    = T.constant "CXCursor_OMPCancelDirective" T.int64_t            
  let omptargetdatadirective                = T.constant "CXCursor_OMPTargetDataDirective" T.int64_t        
  let omptaskloopdirective                  = T.constant "CXCursor_OMPTaskLoopDirective" T.int64_t          
  let omptaskloopsimddirective              = T.constant "CXCursor_OMPTaskLoopSimdDirective" T.int64_t      
  let ompdistributedirective                = T.constant "CXCursor_OMPDistributeDirective" T.int64_t        
  let omptargetenterdatadirective           = T.constant "CXCursor_OMPTargetEnterDataDirective" T.int64_t   
  let omptargetexitdatadirective            = T.constant "CXCursor_OMPTargetExitDataDirective" T.int64_t    
  let omptargetparalleldirective            = T.constant "CXCursor_OMPTargetParallelDirective" T.int64_t    
  let omptargetparallelfordirective         = T.constant "CXCursor_OMPTargetParallelForDirective" T.int64_t 
  let omptargetupdatedirective              = T.constant "CXCursor_OMPTargetUpdateDirective" T.int64_t      
  let ompdistributeparallelfordirective     = T.constant "CXCursor_OMPDistributeParallelForDirective" T.int64_t 
  let ompdistributeparallelforsimddirective = T.constant "CXCursor_OMPDistributeParallelForSimdDirective" T.int64_t 
  let ompdistributesimddirective            = T.constant "CXCursor_OMPDistributeSimdDirective" T.int64_t 
  let omptargetparallelforsimddirective     = T.constant "CXCursor_OMPTargetParallelForSimdDirective" T.int64_t 
  let laststmt                              = T.constant "CXCursor_LastStmt" T.int64_t 
  let translationunit                       = T.constant "CXCursor_TranslationUnit" T.int64_t               
  let firstattr                             = T.constant "CXCursor_FirstAttr" T.int64_t                     
  let unexposedattr                         = T.constant "CXCursor_UnexposedAttr" T.int64_t                 
  let ibactionattr                          = T.constant "CXCursor_IBActionAttr" T.int64_t                  
  let iboutletattr                          = T.constant "CXCursor_IBOutletAttr" T.int64_t                  
  let iboutletcollectionattr                = T.constant "CXCursor_IBOutletCollectionAttr" T.int64_t        
  let cxxfinalattr                          = T.constant "CXCursor_CXXFinalAttr" T.int64_t                  
  let cxxoverrideattr                       = T.constant "CXCursor_CXXOverrideAttr" T.int64_t               
  let annotateattr                          = T.constant "CXCursor_AnnotateAttr" T.int64_t                  
  let asmlabelattr                          = T.constant "CXCursor_AsmLabelAttr" T.int64_t                  
  let packedattr                            = T.constant "CXCursor_PackedAttr" T.int64_t                    
  let pureattr                              = T.constant "CXCursor_PureAttr" T.int64_t                      
  let constattr                             = T.constant "CXCursor_ConstAttr" T.int64_t                     
  let noduplicateattr                       = T.constant "CXCursor_NoDuplicateAttr" T.int64_t               
  let cudaconstantattr                      = T.constant "CXCursor_CUDAConstantAttr" T.int64_t              
  let cudadeviceattr                        = T.constant "CXCursor_CUDADeviceAttr" T.int64_t                
  let cudaglobalattr                        = T.constant "CXCursor_CUDAGlobalAttr" T.int64_t                
  let cudahostattr                          = T.constant "CXCursor_CUDAHostAttr" T.int64_t                  
  let cudasharedattr                        = T.constant "CXCursor_CUDASharedAttr" T.int64_t                
  let visibilityattr                        = T.constant "CXCursor_VisibilityAttr" T.int64_t                
  let dllexport                             = T.constant "CXCursor_DLLExport" T.int64_t                     
  let dllimport                             = T.constant "CXCursor_DLLImport" T.int64_t                     
  let lastattr                              = T.constant "CXCursor_LastAttr" T.int64_t                      
  let preprocessingdirective                = T.constant "CXCursor_PreprocessingDirective" T.int64_t        
  let macrodefinition                       = T.constant "CXCursor_MacroDefinition" T.int64_t               
  let macroexpansion                        = T.constant "CXCursor_MacroExpansion" T.int64_t                
  let macroinstantiation                    = T.constant "CXCursor_MacroInstantiation" T.int64_t            
  let inclusiondirective                    = T.constant "CXCursor_InclusionDirective" T.int64_t            
  let firstpreprocessing                    = T.constant "CXCursor_FirstPreprocessing" T.int64_t            
  let lastpreprocessing                     = T.constant "CXCursor_LastPreprocessing" T.int64_t             
  let moduleimportdecl                      = T.constant "CXCursor_ModuleImportDecl" T.int64_t              
  let typealiastemplatedecl                 = T.constant "CXCursor_TypeAliasTemplateDecl" T.int64_t         
  let staticassert                          = T.constant "CXCursor_StaticAssert" T.int64_t                  
  let firstextradecl                        = T.constant "CXCursor_FirstExtraDecl" T.int64_t                
  let lastextradecl                         = T.constant "CXCursor_LastExtraDecl" T.int64_t                 
  let overloadcandidate                     = T.constant "CXCursor_OverloadCandidate" T.int64_t             



  let cursor_kind = T.enum "CXCursorKind" [
      CXCursor_UnexposedDecl                         , unexposeddecl;
      CXCursor_StructDecl                            , structdecl;
      CXCursor_UnionDecl                             , uniondecl;
      CXCursor_ClassDecl                                      , classdecl;
      CXCursor_EnumDecl                              , enumdecl;
      CXCursor_FieldDecl                             , fielddecl;
      CXCursor_EnumConstantDecl                      , enumconstantdecl;
      CXCursor_FunctionDecl                          , functiondecl;
      CXCursor_VarDecl                               , vardecl;
      CXCursor_ParmDecl                              , parmdecl;
      CXCursor_ObjCInterfaceDecl                     , objcinterfacedecl;
      CXCursor_ObjCCategoryDecl                      , objccategorydecl;
      CXCursor_ObjCProtocolDecl                      , objcprotocoldecl;
      CXCursor_ObjCPropertyDecl                      , objcpropertydecl;
      CXCursor_ObjCIvarDecl                          , objcivardecl;
      CXCursor_ObjCInstanceMethodDecl                , objcinstancemethoddecl;
      CXCursor_ObjCClassMethodDecl                   , objcclassmethoddecl;
      CXCursor_ObjCImplementationDecl                , objcimplementationdecl;
      CXCursor_ObjCCategoryImplDecl                  , objccategoryimpldecl;
      CXCursor_TypedefDecl                           , typedefdecl;
      CXCursor_CXXMethod                             , cxxmethod;
      CXCursor_Namespace                             , namespace;
      CXCursor_LinkageSpec                           , linkagespec;
      CXCursor_Constructor                           , constructor;
      CXCursor_Destructor                            , destructor;
      CXCursor_ConversionFunction                    , conversionfunction;
      CXCursor_TemplateTypeParameter                 , templatetypeparameter;
      CXCursor_NonTypeTemplateParameter              , nontypetemplateparameter;
      CXCursor_TemplateTemplateParameter             , templatetemplateparameter;
      CXCursor_FunctionTemplate                      , functiontemplate;
      CXCursor_ClassTemplate                         , classtemplate;
      CXCursor_ClassTemplatePartialSpecialization    , classtemplatepartialspecialization;
      CXCursor_NamespaceAlias                        , namespacealias;
      CXCursor_UsingDirective                        , usingdirective;
      CXCursor_UsingDeclaration                      , usingdeclaration;
      CXCursor_TypeAliasDecl                         , typealiasdecl;
      CXCursor_ObjCSynthesizeDecl                    , objcsynthesizedecl;
      CXCursor_ObjCDynamicDecl                       , objcdynamicdecl;
      CXCursor_CXXAccessSpecifier                    , cxxaccessspecifier;
      CXCursor_FirstDecl                             , firstdecl;
      CXCursor_LastDecl                              , lastdecl;
      CXCursor_FirstRef                              , firstref;
      CXCursor_ObjCSuperClassRef                     , objcsuperclassref;
      CXCursor_ObjCProtocolRef                       , objcprotocolref;
      CXCursor_ObjCClassRef                          , objcclassref;
      CXCursor_TypeRef                               , typeref;
      CXCursor_CXXBaseSpecifier                      , cxxbasespecifier;
      CXCursor_TemplateRef                           , templateref;
      CXCursor_NamespaceRef                          , namespaceref;
      CXCursor_MemberRef                             , memberref;
      CXCursor_LabelRef                              , labelref;
      CXCursor_OverloadedDeclRef                     , overloadeddeclref;
      CXCursor_VariableRef                           , variableref;
      CXCursor_LastRef                               , lastref;
      CXCursor_FirstInvalid                          , firstinvalid;
      CXCursor_InvalidFile                           , invalidfile;
      CXCursor_NoDeclFound                           , nodeclfound;
      CXCursor_NotImplemented                        , notimplemented;
      CXCursor_InvalidCode                           , invalidcode;
      CXCursor_LastInvalid                           , lastinvalid;
      CXCursor_FirstExpr                             , firstexpr;
      CXCursor_UnexposedExpr                         , unexposedexpr;
      CXCursor_DeclRefExpr                           , declrefexpr;
      CXCursor_MemberRefExpr                         , memberrefexpr;
      CXCursor_CallExpr                              , callexpr;
      CXCursor_ObjCMessageExpr                       , objcmessageexpr;
      CXCursor_BlockExpr                             , blockexpr;
      CXCursor_IntegerLiteral                        , integerliteral;
      CXCursor_FloatingLiteral                       , floatingliteral;
      CXCursor_ImaginaryLiteral                      , imaginaryliteral;
      CXCursor_StringLiteral                         , stringliteral;
      CXCursor_CharacterLiteral                      , characterliteral;
      CXCursor_ParenExpr                             , parenexpr;
      CXCursor_UnaryOperator                         , unaryoperator;
      CXCursor_ArraySubscriptExpr                    , arraysubscriptexpr;
      CXCursor_BinaryOperator                        , binaryoperator;
      CXCursor_CompoundAssignOperator                , compoundassignoperator;
      CXCursor_ConditionalOperator                   , conditionaloperator;
      CXCursor_CStyleCastExpr                        , cstylecastexpr;
      CXCursor_CompoundLiteralExpr                   , compoundliteralexpr;
      CXCursor_InitListExpr                          , initlistexpr;
      CXCursor_AddrLabelExpr                         , addrlabelexpr;
      CXCursor_StmtExpr                              , stmtexpr;
      CXCursor_GenericSelectionExpr                  , genericselectionexpr;
      CXCursor_GNUNullExpr                           , gnunullexpr;
      CXCursor_CXXStaticCastExpr                     , cxxstaticcastexpr;
      CXCursor_CXXDynamicCastExpr                    , cxxdynamiccastexpr;
      CXCursor_CXXReinterpretCastExpr                , cxxreinterpretcastexpr;
      CXCursor_CXXConstCastExpr                      , cxxconstcastexpr;
      CXCursor_CXXFunctionalCastExpr                 , cxxfunctionalcastexpr;
      CXCursor_CXXTypeidExpr                         , cxxtypeidexpr;
      CXCursor_CXXBoolLiteralExpr                    , cxxboolliteralexpr;
      CXCursor_CXXNullPtrLiteralExpr                 , cxxnullptrliteralexpr;
      CXCursor_CXXThisExpr                           , cxxthisexpr;
      CXCursor_CXXThrowExpr                          , cxxthrowexpr;
      CXCursor_CXXNewExpr                            , cxxnewexpr;
      CXCursor_CXXDeleteExpr                         , cxxdeleteexpr;
      CXCursor_UnaryExpr                             , unaryexpr;
      CXCursor_ObjCStringLiteral                     , objcstringliteral;
      CXCursor_ObjCEncodeExpr                        , objcencodeexpr;
      CXCursor_ObjCSelectorExpr                      , objcselectorexpr;
      CXCursor_ObjCProtocolExpr                      , objcprotocolexpr;
      CXCursor_ObjCBridgedCastExpr                   , objcbridgedcastexpr;
      CXCursor_PackExpansionExpr                     , packexpansionexpr;
      CXCursor_SizeOfPackExpr                        , sizeofpackexpr;
      CXCursor_LambdaExpr                            , lambdaexpr;
      CXCursor_ObjCBoolLiteralExpr                   , objcboolliteralexpr;
      CXCursor_ObjCSelfExpr                          , objcselfexpr;
      CXCursor_OMPArraySectionExpr                   , omparraysectionexpr;
      CXCursor_ObjCAvailabilityCheckExpr             , objcavailabilitycheckexpr;
      CXCursor_LastExpr                              , lastexpr;
      CXCursor_FirstStmt                             , firststmt;
      CXCursor_UnexposedStmt                         , unexposedstmt;
      CXCursor_LabelStmt                             , labelstmt;
      CXCursor_CompoundStmt                          , compoundstmt;
      CXCursor_CaseStmt                              , casestmt;
      CXCursor_DefaultStmt                           , defaultstmt;
      CXCursor_IfStmt                                , ifstmt;
      CXCursor_SwitchStmt                            , switchstmt;
      CXCursor_WhileStmt                             , whilestmt;
      CXCursor_DoStmt                                , dostmt;
      CXCursor_ForStmt                               , forstmt;
      CXCursor_GotoStmt                              , gotostmt;
      CXCursor_IndirectGotoStmt                      , indirectgotostmt;
      CXCursor_ContinueStmt                          , continuestmt;
      CXCursor_BreakStmt                             , breakstmt;
      CXCursor_ReturnStmt                            , returnstmt;
      CXCursor_GCCAsmStmt                            , gccasmstmt;
      CXCursor_AsmStmt                               , asmstmt;
      CXCursor_ObjCAtTryStmt                         , objcattrystmt;
      CXCursor_ObjCAtCatchStmt                       , objcatcatchstmt;
      CXCursor_ObjCAtFinallyStmt                     , objcatfinallystmt;
      CXCursor_ObjCAtThrowStmt                       , objcatthrowstmt;
      CXCursor_ObjCAtSynchronizedStmt                , objcatsynchronizedstmt;
      CXCursor_ObjCAutoreleasePoolStmt               , objcautoreleasepoolstmt;
      CXCursor_ObjCForCollectionStmt                 , objcforcollectionstmt;
      CXCursor_CXXCatchStmt                          , cxxcatchstmt;
      CXCursor_CXXTryStmt                            , cxxtrystmt;
      CXCursor_CXXForRangeStmt                       , cxxforrangestmt;
      CXCursor_SEHTryStmt                            , sehtrystmt;
      CXCursor_SEHExceptStmt                         , sehexceptstmt;
      CXCursor_SEHFinallyStmt                        , sehfinallystmt;
      CXCursor_MSAsmStmt                             , msasmstmt;
      CXCursor_NullStmt                              , nullstmt;
      CXCursor_DeclStmt                              , declstmt;
      CXCursor_OMPParallelDirective                  , ompparalleldirective;
      CXCursor_OMPSimdDirective                      , ompsimddirective;
      CXCursor_OMPForDirective                       , ompfordirective;
      CXCursor_OMPSectionsDirective                  , ompsectionsdirective;
      CXCursor_OMPSectionDirective                   , ompsectiondirective;
      CXCursor_OMPSingleDirective                    , ompsingledirective;
      CXCursor_OMPParallelForDirective               , ompparallelfordirective;
      CXCursor_OMPParallelSectionsDirective          , ompparallelsectionsdirective;
      CXCursor_OMPTaskDirective                      , omptaskdirective;
      CXCursor_OMPMasterDirective                    , ompmasterdirective;
      CXCursor_OMPCriticalDirective                  , ompcriticaldirective;
      CXCursor_OMPTaskyieldDirective                 , omptaskyielddirective;
      CXCursor_OMPBarrierDirective                   , ompbarrierdirective;
      CXCursor_OMPTaskwaitDirective                  , omptaskwaitdirective;
      CXCursor_OMPFlushDirective                     , ompflushdirective;
      CXCursor_SEHLeaveStmt                          , sehleavestmt;
      CXCursor_OMPOrderedDirective                   , ompordereddirective;
      CXCursor_OMPAtomicDirective                    , ompatomicdirective;
      CXCursor_OMPForSimdDirective                   , ompforsimddirective;
      CXCursor_OMPParallelForSimdDirective           , ompparallelforsimddirective;
      CXCursor_OMPTargetDirective                    , omptargetdirective;
      CXCursor_OMPTeamsDirective                     , ompteamsdirective;
      CXCursor_OMPTaskgroupDirective                 , omptaskgroupdirective;
      CXCursor_OMPCancellationPointDirective         , ompcancellationpointdirective;
      CXCursor_OMPCancelDirective                    , ompcanceldirective;
      CXCursor_OMPTargetDataDirective                , omptargetdatadirective;
      CXCursor_OMPTaskLoopDirective                  , omptaskloopdirective;
      CXCursor_OMPTaskLoopSimdDirective              , omptaskloopsimddirective;
      CXCursor_OMPDistributeDirective                , ompdistributedirective;
      CXCursor_OMPTargetEnterDataDirective           , omptargetenterdatadirective;
      CXCursor_OMPTargetExitDataDirective            , omptargetexitdatadirective;
      CXCursor_OMPTargetParallelDirective            , omptargetparalleldirective;
      CXCursor_OMPTargetParallelForDirective         , omptargetparallelfordirective;
      CXCursor_OMPTargetUpdateDirective              , omptargetupdatedirective;
      CXCursor_OMPDistributeParallelForDirective     , ompdistributeparallelfordirective;
      CXCursor_OMPDistributeParallelForSimdDirective , ompdistributeparallelforsimddirective;
      CXCursor_OMPDistributeSimdDirective            , ompdistributesimddirective;
      CXCursor_OMPTargetParallelForSimdDirective     , omptargetparallelforsimddirective;
      CXCursor_LastStmt                              , laststmt;
      CXCursor_TranslationUnit                       , translationunit;
      CXCursor_FirstAttr                             , firstattr;
      CXCursor_UnexposedAttr                         , unexposedattr;
      CXCursor_IBActionAttr                          , ibactionattr;
      CXCursor_IBOutletAttr                          , iboutletattr;
      CXCursor_IBOutletCollectionAttr                , iboutletcollectionattr;
      CXCursor_CXXFinalAttr                          , cxxfinalattr;
      CXCursor_CXXOverrideAttr                       , cxxoverrideattr;
      CXCursor_AnnotateAttr                          , annotateattr;
      CXCursor_AsmLabelAttr                          , asmlabelattr;
      CXCursor_PackedAttr                            , packedattr;
      CXCursor_PureAttr                              , pureattr;
      CXCursor_ConstAttr                             , constattr;
      CXCursor_NoDuplicateAttr                       , noduplicateattr;
      CXCursor_CUDAConstantAttr                      , cudaconstantattr;
      CXCursor_CUDADeviceAttr                        , cudadeviceattr;
      CXCursor_CUDAGlobalAttr                        , cudaglobalattr;
      CXCursor_CUDAHostAttr                          , cudahostattr;
      CXCursor_CUDASharedAttr                        , cudasharedattr;
      CXCursor_VisibilityAttr                        , visibilityattr;
      CXCursor_DLLExport                             , dllexport;
      CXCursor_DLLImport                             , dllimport;
      CXCursor_LastAttr                              , lastattr;
      CXCursor_PreprocessingDirective                , preprocessingdirective;
      CXCursor_MacroDefinition                       , macrodefinition;
      CXCursor_MacroExpansion                        , macroexpansion;
      CXCursor_MacroInstantiation                    , macroinstantiation;
      CXCursor_InclusionDirective                    , inclusiondirective;
      CXCursor_FirstPreprocessing                    , firstpreprocessing;
      CXCursor_LastPreprocessing                     , lastpreprocessing;
      CXCursor_ModuleImportDecl                      , moduleimportdecl;
      CXCursor_TypeAliasTemplateDecl                 , typealiastemplatedecl;
      CXCursor_StaticAssert                          , staticassert;
      CXCursor_FirstExtraDecl                        , firstextradecl;
      CXCursor_LastExtraDecl                         , lastextradecl;
      CXCursor_OverloadCandidate, overloadcandidate;
    ]


  let visitbreak = T.constant "CXChildVisit_Break" T.int64_t
  let visitcontinue = T.constant "CXChildVisit_Continue" T.int64_t
  let visitrecurse = T.constant "CXChildVisit_Recurse" T.int64_t

  let cx_child_visit_result = T.enum "CXChildVisitResult" [
      CXChildVisitBreak, visitbreak;
      CXChildVisitContinue, visitcontinue;
      CXChildVisitRecurse, visitrecurse;

    ]

  let cx_type : cx_type Ctypes.structure T.typ = T.structure "_CXType"
  let cx_type = T.typedef cx_type "CXType"
  let cx_type_kind = T.field cx_type "kind" kind
  let d = T.field cx_type "data" (T.array 2 (T.ptr T.void))
  let () = T.seal cx_type

  let cx_cursor : cx_cursor Ctypes.structure T.typ = T.structure "_CXCursor"
  let cx_cursor = T.typedef cx_cursor "CXCursor" 
  let cx_cursor_kind = T.field cx_cursor "kind" cursor_kind
  let xdata = T.field cx_cursor "xdata" T.int
  (*let data = T.field cx_cursor "data" (T.array 3 (T.ptr T.void)) *)
  let data = Array.init 3 (fun i -> T.field cx_cursor ("data") (T.ptr T.void))
  let () = T.seal cx_cursor 


  let cx_client_data : cx_client_data T.typ = T.ptr T.void 
  
  let cx_translation_unit : cx_translation_unit T.typ = T.ptr T.void

  let cx_index : cx_index T.typ = (T.ptr T.void)

end



module Bindings (T : Cstubs_structs.TYPE with type 'a typ = 'a typ)(F : Cstubs.FOREIGN) =
struct
  (* module T = Ffi_types.Types(Ffi_generated_types) *)

  module E = Enums (T)
  open F

  let getcstring_ = F.foreign "clang_getCString" (E.cx_string @-> returning T.string)

  let disposecstring_ = F.foreign "clang_disposeString" (E.cx_string @-> returning T.void)
                  
  let version_ = F.foreign "clang_getClangVersion" (void @-> returning E.cx_string)

  let toggle_crash_recovery_ = F.foreign "clang_toggleCrashRecovery" (bool @-> returning T.void)

  let get_type_spelling = foreign "clang_getTypeSpelling" (E.cx_type @-> returning E.cx_string)

  let create_index_ = F.foreign "clang_createIndex" (int @-> int @-> returning E.cx_index)
                        
  let create_translation_unit_from_source_ = F.foreign "clang_createTranslationUnitFromSourceFile" (E.cx_index @-> string @-> int @-> string @-> int @-> ptr void @-> returning E.cx_translation_unit)

  let get_tu_spelling = F.foreign "clang_getTranslationUnitSpelling" (E.cx_translation_unit @-> returning E.cx_string)

  let cursor_of_translation_unit_ = F.foreign "clang_getTranslationUnitCursor" (E.cx_translation_unit @-> returning E.cx_cursor)

  let visitor_type = Ctypes.static_funptr T.(E.cx_cursor @-> E.cx_cursor @-> E.cx_client_data @-> returning E.cx_child_visit_result)
  let visitor_typep = T.typedef (visitor_type) "CXCursorVisitor"
                                  
  let visit_children_ = F.foreign "clang_visitChildren" (E.cx_cursor @-> visitor_typep @-> E.cx_client_data @-> returning uint)

  let get_cursor_spelling = F.foreign "clang_getCursorSpelling" (E.cx_cursor @-> returning E.cx_string)

  let get_display_name = F.foreign "clang_getCursorDisplayName" (E.cx_cursor @-> returning E.cx_string)

  let cursor_is_null = F.foreign "clang_Cursor_isNull" (E.cx_cursor @-> returning int)

  let cx_cursor_visitor = T.(E.cx_cursor @-> E.cx_cursor @-> E.cx_client_data @-> returning E.cx_child_visit_result)
  let cx_get_cursor_kind = F.foreign "clang_getCursorKind" F.(E.cx_cursor @-> returning E.cursor_kind)

  let clang_gettypedef = F.foreign "clang_getTypedefDeclUnderlyingType" F.(E.cx_cursor @-> returning E.cx_type)

  let clang_getresulttype = F.foreign "clang_getResultType" F.(E.cx_type @-> returning E.cx_type)

  let clang_of_cursor = F.foreign "clang_getCursorType" F.(E.cx_cursor @-> returning E.cx_type)

  let clang_get_pointee_type = F.foreign "clang_getPointeeType" F.(E.cx_type @-> returning E.cx_type)

  let clang_cxtype_resolve_typedef = F.foreign "clang_getTypedefDeclUnderlyingType" F.(E.cx_cursor @-> returning E.cx_type)

  let clang_get_field_bit_width = F.foreign "clang_getFieldDeclBitWidth" F.(E.cx_cursor @-> returning int)

  let clang_get_canonical_type = F.foreign "clang_getCanonicalType" F.(E.cx_type @-> returning E.cx_type)
  
  
end
