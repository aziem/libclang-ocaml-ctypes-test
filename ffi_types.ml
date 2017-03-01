
module Types (F : Cstubs_structs.TYPE) = 
struct

  type cx_string
  let cx_string : cx_string Ctypes.structure F.typ = F.structure "_CXString"
  let cx_string = F.typedef cx_string "CXString"
  let cx_string_data = F.field cx_string "data" (F.ptr F.void) 
  let cx_string_private_flags = F.field cx_string "private_flags" F.uint 
  let () = F.seal cx_string 


  type cx_error_code = 
    | CXError_Success
    | CXError_Failure
    | CXError_Crashed
    | CXError_InvalidArguments
    | CXError_ASTReadError

  let cxerror_success = F.constant "CXError_Success" F.int64_t
  let cxerror_failure = F.constant "CXError_Failure" F.int64_t
  let cxerror_crashed = F.constant "CXError_Crashed" F.int64_t
  let cxerror_invalidarguments = F.constant "CXError_InvalidArguments" F.int64_t
  let cxerror_astreaderror = F.constant "CXError_ASTReadError" F.int64_t

  let cx_error_code = F.enum "CXErrorCode" [
      CXError_Success, cxerror_success;
      CXError_Failure, cxerror_failure;
      CXError_Crashed, cxerror_crashed;
      CXError_InvalidArguments, cxerror_invalidarguments;
      CXError_ASTReadError, cxerror_astreaderror
    ]

  type kind =
    | Invalid
    | Unexposed
    | Void
    | Bool
    | Char_U
    | UChar
    | Char16
    | Char32
    | UShort
    | UInt
    | ULong
    | ULongLong
    | UInt128
    | Char_S
    | SChar
    | WChar
    | Short
    | Int
    | Long
    | LongLong
    | Int128
    | Float
    | Double
    | LongDouble
    | NullPtr
    | Overload
    | Dependent
    | ObjCId
    | ObjCClass
    | ObjCSel
    | FirstBuiltin
    | LastBuiltin
    | Complex
    | Pointer
    | BlockPointer
    | LValueReference
    | RValueReference
    | Record
    | Enum
    | Typedef
    | ObjCInterface
    | ObjCObjectPointer
    | FunctionNoProto
    | FunctionProto
    | ConstantArray
    | Vector
    | IncompleteArray
    | VariableArray
    | DependentSizedArray
    | MemberPointer
    | Auto
    | Elaborated


  let invalid             = F.constant "CXType_Invalid" F.int64_t
  let unexposed           = F.constant "CXType_Unexposed" F.int64_t
  let cxvoid              = F.constant "CXType_Void" F.int64_t
  let bool                = F.constant "CXType_Bool" F.int64_t
  let char_u              = F.constant "CXType_Char_U" F.int64_t
  let uchar               = F.constant "CXType_UChar" F.int64_t
  let char16              = F.constant "CXType_Char16" F.int64_t
  let char32              = F.constant "CXType_Char32" F.int64_t
  let ushort              = F.constant "CXType_UShort" F.int64_t
  let uint                = F.constant "CXType_UInt" F.int64_t
  let ulong               = F.constant "CXType_ULong" F.int64_t
  let ulonglong           = F.constant "CXType_ULongLong" F.int64_t
  let uint128             = F.constant "CXType_UInt128" F.int64_t
  let char_s              = F.constant "CXType_Char_S" F.int64_t
  let schar               = F.constant "CXType_SChar" F.int64_t
  let wchar               = F.constant "CXType_WChar" F.int64_t
  let short               = F.constant "CXType_Short" F.int64_t
  let int                 = F.constant "CXType_Int" F.int64_t
  let long                = F.constant "CXType_Long" F.int64_t
  let longlong            = F.constant "CXType_LongLong" F.int64_t
  let int128              = F.constant "CXType_Int128" F.int64_t
  let float               = F.constant "CXType_Float" F.int64_t
  let double              = F.constant "CXType_Double" F.int64_t
  let longdouble          = F.constant "CXType_LongDouble" F.int64_t
  let nullptr             = F.constant "CXType_NullPtr" F.int64_t
  let overload            = F.constant "CXType_Overload" F.int64_t
  let dependent           = F.constant "CXType_Dependent" F.int64_t
  let objcid              = F.constant "CXType_ObjCId" F.int64_t
  let objcclass           = F.constant "CXType_ObjCClass" F.int64_t
  let objcsel             = F.constant "CXType_ObjCSel" F.int64_t
  let float128            = F.constant "CXType_Float128" F.int64_t
  let firstbuiltin        = F.constant "CXType_FirstBuiltin" F.int64_t
  let lastbuiltin         = F.constant "CXType_LastBuiltin"  F.int64_t
  let complex             = F.constant "CXType_Complex" F.int64_t
  let pointer             = F.constant "CXType_Pointer" F.int64_t
  let blockpointer        = F.constant "CXType_BlockPointer" F.int64_t
  let lvaluereference     = F.constant "CXType_LValueReference" F.int64_t
  let rvaluereference     = F.constant "CXType_RValueReference" F.int64_t
  let record              = F.constant "CXType_Record" F.int64_t
  let cxenum              = F.constant "CXType_Enum" F.int64_t
  let typedef             = F.constant "CXType_Typedef" F.int64_t
  let objcinterface       = F.constant "CXType_ObjCInterface" F.int64_t
  let objcobjectpointer   = F.constant "CXType_ObjCObjectPointer" F.int64_t
  let functionnoproto     = F.constant "CXType_FunctionNoProto" F.int64_t
  let functionproto       = F.constant "CXType_FunctionProto" F.int64_t
  let constantarray       = F.constant "CXType_ConstantArray" F.int64_t
  let vector              = F.constant "CXType_Vector" F.int64_t
  let incompletearray     = F.constant "CXType_IncompleteArray" F.int64_t
  let variablearray       = F.constant "CXType_VariableArray" F.int64_t
  let dependentsizedarray = F.constant "CXType_DependentSizedArray" F.int64_t
  let memberpointer       = F.constant "CXType_MemberPointer" F.int64_t
  let auto                = F.constant "CXType_Auto" F.int64_t
  let elaborated          = F.constant "CXType_Elaborated" F.int64_t

  let kind = F.enum "CXTypeKind" [
      Invalid, invalid;
      Unexposed, unexposed;
      Void, cxvoid;
      Bool, bool;
      Char_U, char_u;
      UChar, uchar;
      Char16, char16;
      Char32, char32;
      UShort, ushort;
      UInt, uint;
      ULong, ulong;
      ULongLong, ulonglong;
      UInt128, uint128;
      Char_S, char_s;
      SChar, schar;
      WChar, wchar;
      Short, short;
      Int, int;
      Long, long;
      LongLong, longlong;
      Int128, int128;
      Float, float;
      Double, double;
      LongDouble, longdouble;
      NullPtr, nullptr;
      Overload, overload;
      Dependent, dependent;
      ObjCId, objcid;
      ObjCClass, objcclass;
      ObjCSel, objcsel;
      FirstBuiltin, firstbuiltin;
      LastBuiltin, lastbuiltin;
      Complex, complex;
      Pointer, pointer;
      BlockPointer, blockpointer;
      LValueReference, lvaluereference;
      RValueReference, rvaluereference;
      Record, record;
      Enum, cxenum;
      Typedef, typedef;
      ObjCInterface, objcinterface;
      ObjCObjectPointer, objcobjectpointer;
      FunctionNoProto, functionnoproto;
      FunctionProto, functionproto;
      ConstantArray, constantarray;
      Vector, vector;
      IncompleteArray, incompletearray;
      VariableArray, variablearray;
      DependentSizedArray, dependentsizedarray;
      MemberPointer, memberpointer;
      Auto, auto;
      Elaborated, elaborated;
    ]


  type calling_conv =
    | CallingConv_Default
    | CallingConv_C
    | CallingConv_X86StdCall
    | CallingConv_X86FastCall
    | CallingConv_X86ThisCall
    | CallingConv_X86Pascal
    | CallingConv_AAPCS
    | CallingConv_AAPCS_VFP
    | CallingConv_IntelOclBicc
    | CallingConv_X86_64Win64
    | CallingConv_X86_64SysV
    | CallingConv_Invalid
    | CallingConv_Unexposed

  let cxcallingconv_default       = F.constant "CXCallingConv_Default" F.int64_t
  let cxcallingconv_c             = F.constant "CXCallingConv_C" F.int64_t
  let cxcallingconv_x86stdcall    = F.constant "CXCallingConv_X86StdCall" F.int64_t
  let cxcallingconv_x86fastcall   = F.constant "CXCallingConv_X86FastCall" F.int64_t
  let cxcallingconv_x86thiscall   = F.constant "CXCallingConv_X86ThisCall" F.int64_t
  let cxcallingconv_x86pascal     = F.constant "CXCallingConv_X86Pascal" F.int64_t
  let cxcallingconv_aapcs         = F.constant "CXCallingConv_AAPCS" F.int64_t
  let cxcallingconv_aapcs_vfp     = F.constant "CXCallingConv_AAPCS_VFP" F.int64_t
  let cxcallingconv_inteloclbicc  = F.constant "CXCallingConv_IntelOclBicc" F.int64_t
  let cxcallingconv_x86_64win64   = F.constant "CXCallingConv_X86_64Win64" F.int64_t
  let cxcallingconv_x86_64sysv    = F.constant "CXCallingConv_X86_64SysV" F.int64_t
  let cxcallingconv_x86vectorcall = F.constant "CXCallingConv_X86VectorCall" F.int64_t
  let cxcallingconv_swift         = F.constant "CXCallingConv_Swift" F.int64_t
  let cxcallingconv_preservemost  = F.constant "CXCallingConv_PreserveMost" F.int64_t
  let cxcallingconv_preserveall   = F.constant "CXCallingConv_PreserveAll" F.int64_t
  let cxcallingconv_invalid       = F.constant "CXCallingConv_Invalid" F.int64_t
  let cxcallingconv_unexposed     = F.constant "CXCallingConv_Unexposed" F.int64_t

  let calling_conv = F.enum "CXCallingConv" [
      CallingConv_Default, cxcallingconv_default;
      CallingConv_C, cxcallingconv_c;
      CallingConv_X86StdCall, cxcallingconv_x86stdcall;
      CallingConv_X86FastCall, cxcallingconv_x86fastcall;
      CallingConv_X86ThisCall, cxcallingconv_x86thiscall;
      CallingConv_X86Pascal, cxcallingconv_x86pascal;
      CallingConv_AAPCS, cxcallingconv_aapcs;
      CallingConv_AAPCS_VFP, cxcallingconv_aapcs_vfp;
      CallingConv_IntelOclBicc, cxcallingconv_inteloclbicc;
      CallingConv_X86_64Win64, cxcallingconv_x86_64win64;
      CallingConv_X86_64SysV, cxcallingconv_x86_64sysv;
      CallingConv_Invalid, cxcallingconv_invalid;
      CallingConv_Unexposed, cxcallingconv_unexposed;
    ]

  type layout_error =
    | InvalidLayout
    | IncompleteLayout
    | DependentLayout
    | NotConstantSizeLayout
    | InvalidFieldNameLayout

  let invalidlayout          = F.constant "CXTypeLayoutError_Invalid"  F.int64_t
  let incompletelayout       = F.constant "CXTypeLayoutError_Incomplete"  F.int64_t
  let dependentlayout        = F.constant "CXTypeLayoutError_Dependent"  F.int64_t
  let notconstantsizelayout  = F.constant "CXTypeLayoutError_NotConstantSize"  F.int64_t
  let invalidfieldnamelayout = F.constant "CXTypeLayoutError_InvalidFieldName"  F.int64_t

  let layout_error = F.enum "CXTypeLayoutError" [
      InvalidLayout, invalidlayout;
      IncompleteLayout, incompletelayout;
      DependentLayout, dependentlayout;
      NotConstantSizeLayout, notconstantsizelayout;
      InvalidFieldNameLayout, invalidfieldnamelayout;
    ]


  type translation_unit_options =
    | NoOptions
    | DetailedPreprocessingRecord
    | Incomplete
    | PrecompiledPreamble
    | CacheCompletionResults
    | ForSerialization
    | CXXChainedPCH
    | SkipFunctionBodies
    | IncludeBriefCommentsInCodeCompletion
    | CreatePreambleOnFirstParse
    | KeepGoing

  let nooptions                            = F.constant "CXTranslationUnit_None" F.int64_t
  let detailedpreprocessingrecord          = F.constant "CXTranslationUnit_DetailedPreprocessingRecord" F.int64_t
  let incomplete                           = F.constant "CXTranslationUnit_Incomplete" F.int64_t
  let precompiledpreamble                  = F.constant "CXTranslationUnit_PrecompiledPreamble" F.int64_t
  let cachecompletionresults               = F.constant "CXTranslationUnit_CacheCompletionResults" F.int64_t
  let forserialization                     = F.constant "CXTranslationUnit_ForSerialization" F.int64_t
  let cxxchainedpch                        = F.constant "CXTranslationUnit_CXXChainedPCH" F.int64_t
  let skipfunctionbodies                   = F.constant "CXTranslationUnit_SkipFunctionBodies" F.int64_t
  let includebriefcommentsincodecompletion = F.constant "CXTranslationUnit_IncludeBriefCommentsInCodeCompletion" F.int64_t
  let createpreambleonfirstparse           = F.constant "CXTranslationUnit_CreatePreambleOnFirstParse" F.int64_t
  let keepgoing                            = F.constant "CXTranslationUnit_KeepGoing" F.int64_t

  let translation_unit_options = F.enum "CXTranslationUnit_Flags"
      [
        NoOptions, nooptions;
        DetailedPreprocessingRecord, detailedpreprocessingrecord;
        Incomplete, incomplete;
        PrecompiledPreamble, precompiledpreamble;
        CacheCompletionResults, cachecompletionresults;
        ForSerialization, forserialization;
        CXXChainedPCH, cxxchainedpch;
        SkipFunctionBodies, skipfunctionbodies;
        IncludeBriefCommentsInCodeCompletion, includebriefcommentsincodecompletion;
        CreatePreambleOnFirstParse, createpreambleonfirstparse;
        KeepGoing, keepgoing;
      ]
  type cursor_kind = 
    | UnexposedDecl                 
    | StructDecl                    
    | UnionDecl                     
    | ClassDecl                     
    | EnumDecl                      
    | FieldDecl                     
    | EnumConstantDecl              
    | FunctionDecl                  
    | VarDecl                       
    | ParmDecl                      
    | ObjCInterfaceDecl             
    | ObjCCategoryDecl              
    | ObjCProtocolDecl              
    | ObjCPropertyDecl              
    | ObjCIvarDecl                  
    | ObjCInstanceMethodDecl        
    | ObjCClassMethodDecl           
    | ObjCImplementationDecl        
    | ObjCCategoryImplDecl          
    | TypedefDecl                   
    | CXXMethod                     
    | Namespace                     
    | LinkageSpec                   
    | Constructor                   
    | Destructor                    
    | ConversionFunction            
    | TemplateTypeParameter         
    | NonTypeTemplateParameter      
    | TemplateTemplateParameter     
    | FunctionTemplate              
    | ClassTemplate                 
    | ClassTemplatePartialSpecialization 
    | NamespaceAlias                
    | UsingDirective                
    | UsingDeclaration              
    | TypeAliasDecl                 
    | ObjCSynthesizeDecl            
    | ObjCDynamicDecl               
    | CXXAccessSpecifier            
    | FirstDecl                     
    | LastDecl                      
    | FirstRef                      
    | ObjCSuperClassRef             
    | ObjCProtocolRef               
    | ObjCClassRef                  
    | TypeRef                       
    | CXXBaseSpecifier              
    | TemplateRef                   
    | NamespaceRef                  
    | MemberRef                     
    | LabelRef                      
    | OverloadedDeclRef             
    | VariableRef                   
    | LastRef                       
    | FirstInvalid                  
    | InvalidFile                   
    | NoDeclFound                   
    | NotImplemented                
    | InvalidCode                   
    | LastInvalid                   
    | FirstExpr                     
    | UnexposedExpr                 
    | DeclRefExpr                   
    | MemberRefExpr                 
    | CallExpr                      
    | ObjCMessageExpr               
    | BlockExpr                     
    | IntegerLiteral                
    | FloatingLiteral               
    | ImaginaryLiteral              
    | StringLiteral                 
    | CharacterLiteral              
    | ParenExpr                     
    | UnaryOperator                 
    | ArraySubscriptExpr            
    | BinaryOperator                
    | CompoundAssignOperator        
    | ConditionalOperator           
    | CStyleCastExpr                
    | CompoundLiteralExpr           
    | InitListExpr                  
    | AddrLabelExpr                 
    | StmtExpr                      
    | GenericSelectionExpr          
    | GNUNullExpr                   
    | CXXStaticCastExpr             
    | CXXDynamicCastExpr            
    | CXXReinterpretCastExpr        
    | CXXConstCastExpr              
    | CXXFunctionalCastExpr         
    | CXXTypeidExpr                 
    | CXXBoolLiteralExpr            
    | CXXNullPtrLiteralExpr         
    | CXXThisExpr                   
    | CXXThrowExpr                  
    | CXXNewExpr                    
    | CXXDeleteExpr                 
    | UnaryExpr                     
    | ObjCStringLiteral             
    | ObjCEncodeExpr                
    | ObjCSelectorExpr              
    | ObjCProtocolExpr              
    | ObjCBridgedCastExpr           
    | PackExpansionExpr             
    | SizeOfPackExpr                
    | LambdaExpr                    
    | ObjCBoolLiteralExpr           
    | ObjCSelfExpr                  
    | OMPArraySectionExpr           
    | ObjCAvailabilityCheckExpr     
    | LastExpr                      
    | FirstStmt                     
    | UnexposedStmt                 
    | LabelStmt                     
    | CompoundStmt                  
    | CaseStmt                      
    | DefaultStmt                   
    | IfStmt                        
    | SwitchStmt                    
    | WhileStmt                     
    | DoStmt                        
    | ForStmt                       
    | GotoStmt                      
    | IndirectGotoStmt              
    | ContinueStmt                  
    | BreakStmt                     
    | ReturnStmt                    
    | GCCAsmStmt                    
    | AsmStmt                       
    | ObjCAtTryStmt                 
    | ObjCAtCatchStmt               
    | ObjCAtFinallyStmt             
    | ObjCAtThrowStmt               
    | ObjCAtSynchronizedStmt        
    | ObjCAutoreleasePoolStmt       
    | ObjCForCollectionStmt         
    | CXXCatchStmt                  
    | CXXTryStmt                    
    | CXXForRangeStmt               
    | SEHTryStmt                    
    | SEHExceptStmt                 
    | SEHFinallyStmt                
    | MSAsmStmt                     
    | NullStmt                      
    | DeclStmt                      
    | OMPParallelDirective          
    | OMPSimdDirective              
    | OMPForDirective               
    | OMPSectionsDirective          
    | OMPSectionDirective           
    | OMPSingleDirective            
    | OMPParallelForDirective       
    | OMPParallelSectionsDirective  
    | OMPTaskDirective              
    | OMPMasterDirective            
    | OMPCriticalDirective          
    | OMPTaskyieldDirective         
    | OMPBarrierDirective           
    | OMPTaskwaitDirective          
    | OMPFlushDirective             
    | SEHLeaveStmt                  
    | OMPOrderedDirective           
    | OMPAtomicDirective            
    | OMPForSimdDirective           
    | OMPParallelForSimdDirective   
    | OMPTargetDirective            
    | OMPTeamsDirective             
    | OMPTaskgroupDirective         
    | OMPCancellationPointDirective 
    | OMPCancelDirective            
    | OMPTargetDataDirective        
    | OMPTaskLoopDirective          
    | OMPTaskLoopSimdDirective      
    | OMPDistributeDirective        
    | OMPTargetEnterDataDirective   
    | OMPTargetExitDataDirective    
    | OMPTargetParallelDirective    
    | OMPTargetParallelForDirective 
    | OMPTargetUpdateDirective      
    | OMPDistributeParallelForDirective 
    | OMPDistributeParallelForSimdDirective 
    | OMPDistributeSimdDirective 
    | OMPTargetParallelForSimdDirective 
    | LastStmt 
    | TranslationUnit               
    | FirstAttr                     
    | UnexposedAttr                 
    | IBActionAttr                  
    | IBOutletAttr                  
    | IBOutletCollectionAttr        
    | CXXFinalAttr                  
    | CXXOverrideAttr               
    | AnnotateAttr                  
    | AsmLabelAttr                  
    | PackedAttr                    
    | PureAttr                      
    | ConstAttr                     
    | NoDuplicateAttr               
    | CUDAConstantAttr              
    | CUDADeviceAttr                
    | CUDAGlobalAttr                
    | CUDAHostAttr                  
    | CUDASharedAttr                
    | VisibilityAttr                
    | DLLExport                     
    | DLLImport                     
    | LastAttr                      
    | PreprocessingDirective        
    | MacroDefinition               
    | MacroExpansion                
    | MacroInstantiation            
    | InclusionDirective            
    | FirstPreprocessing            
    | LastPreprocessing             
    | ModuleImportDecl              
    | TypeAliasTemplateDecl         
    | StaticAssert                  
    | FirstExtraDecl                
    | LastExtraDecl                 
    | OverloadCandidate             

  let unexposeddecl                         = F.constant "CXCursor_UnexposedDecl" F.int64_t                 
  let structdecl                            = F.constant "CXCursor_StructDecl" F.int64_t                    
  let uniondecl                             = F.constant "CXCursor_UnionDecl" F.int64_t                     
  let classdecl                             = F.constant "CXCursor_ClassDecl" F.int64_t                     
  let enumdecl                              = F.constant "CXCursor_EnumDecl" F.int64_t                      
  let fielddecl                             = F.constant "CXCursor_FieldDecl" F.int64_t                     
  let enumconstantdecl                      = F.constant "CXCursor_EnumConstantDecl" F.int64_t              
  let functiondecl                          = F.constant "CXCursor_FunctionDecl" F.int64_t                  
  let vardecl                               = F.constant "CXCursor_VarDecl" F.int64_t                       
  let parmdecl                              = F.constant "CXCursor_ParmDecl" F.int64_t                      
  let objcinterfacedecl                     = F.constant "CXCursor_ObjCInterfaceDecl" F.int64_t             
  let objccategorydecl                      = F.constant "CXCursor_ObjCCategoryDecl" F.int64_t              
  let objcprotocoldecl                      = F.constant "CXCursor_ObjCProtocolDecl" F.int64_t              
  let objcpropertydecl                      = F.constant "CXCursor_ObjCPropertyDecl" F.int64_t              
  let objcivardecl                          = F.constant "CXCursor_ObjCIvarDecl" F.int64_t                  
  let objcinstancemethoddecl                = F.constant "CXCursor_ObjCInstanceMethodDecl" F.int64_t        
  let objcclassmethoddecl                   = F.constant "CXCursor_ObjCClassMethodDecl" F.int64_t           
  let objcimplementationdecl                = F.constant "CXCursor_ObjCImplementationDecl" F.int64_t        
  let objccategoryimpldecl                  = F.constant "CXCursor_ObjCCategoryImplDecl" F.int64_t          
  let typedefdecl                           = F.constant "CXCursor_TypedefDecl" F.int64_t                   
  let cxxmethod                             = F.constant "CXCursor_CXXMethod" F.int64_t                     
  let namespace                             = F.constant "CXCursor_Namespace" F.int64_t                     
  let linkagespec                           = F.constant "CXCursor_LinkageSpec" F.int64_t                   
  let constructor                           = F.constant "CXCursor_Constructor" F.int64_t                   
  let destructor                            = F.constant "CXCursor_Destructor" F.int64_t                    
  let conversionfunction                    = F.constant "CXCursor_ConversionFunction" F.int64_t            
  let templatetypeparameter                 = F.constant "CXCursor_TemplateTypeParameter" F.int64_t         
  let nontypetemplateparameter              = F.constant "CXCursor_NonTypeTemplateParameter" F.int64_t      
  let templatetemplateparameter             = F.constant "CXCursor_TemplateTemplateParameter" F.int64_t     
  let functiontemplate                      = F.constant "CXCursor_FunctionTemplate" F.int64_t              
  let classtemplate                         = F.constant "CXCursor_ClassTemplate" F.int64_t                 
  let classtemplatepartialspecialization    = F.constant "CXCursor_ClassTemplatePartialSpecialization" F.int64_t 
  let namespacealias                        = F.constant "CXCursor_NamespaceAlias" F.int64_t                
  let usingdirective                        = F.constant "CXCursor_UsingDirective" F.int64_t                
  let usingdeclaration                      = F.constant "CXCursor_UsingDeclaration" F.int64_t              
  let typealiasdecl                         = F.constant "CXCursor_TypeAliasDecl" F.int64_t                 
  let objcsynthesizedecl                    = F.constant "CXCursor_ObjCSynthesizeDecl" F.int64_t            
  let objcdynamicdecl                       = F.constant "CXCursor_ObjCDynamicDecl" F.int64_t               
  let cxxaccessspecifier                    = F.constant "CXCursor_CXXAccessSpecifier" F.int64_t            
  let firstdecl                             = F.constant "CXCursor_FirstDecl" F.int64_t                     
  let lastdecl                              = F.constant "CXCursor_LastDecl" F.int64_t                      
  let firstref                              = F.constant "CXCursor_FirstRef" F.int64_t                      
  let objcsuperclassref                     = F.constant "CXCursor_ObjCSuperClassRef" F.int64_t             
  let objcprotocolref                       = F.constant "CXCursor_ObjCProtocolRef" F.int64_t               
  let objcclassref                          = F.constant "CXCursor_ObjCClassRef" F.int64_t                  
  let typeref                               = F.constant "CXCursor_TypeRef" F.int64_t                       
  let cxxbasespecifier                      = F.constant "CXCursor_CXXBaseSpecifier" F.int64_t              
  let templateref                           = F.constant "CXCursor_TemplateRef" F.int64_t                   
  let namespaceref                          = F.constant "CXCursor_NamespaceRef" F.int64_t                  
  let memberref                             = F.constant "CXCursor_MemberRef" F.int64_t                     
  let labelref                              = F.constant "CXCursor_LabelRef" F.int64_t                      
  let overloadeddeclref                     = F.constant "CXCursor_OverloadedDeclRef" F.int64_t             
  let variableref                           = F.constant "CXCursor_VariableRef" F.int64_t                   
  let lastref                               = F.constant "CXCursor_LastRef" F.int64_t                       
  let firstinvalid                          = F.constant "CXCursor_FirstInvalid" F.int64_t                  
  let invalidfile                           = F.constant "CXCursor_InvalidFile" F.int64_t                   
  let nodeclfound                           = F.constant "CXCursor_NoDeclFound" F.int64_t                   
  let notimplemented                        = F.constant "CXCursor_NotImplemented" F.int64_t                
  let invalidcode                           = F.constant "CXCursor_InvalidCode" F.int64_t                   
  let lastinvalid                           = F.constant "CXCursor_LastInvalid" F.int64_t                   
  let firstexpr                             = F.constant "CXCursor_FirstExpr" F.int64_t                     
  let unexposedexpr                         = F.constant "CXCursor_UnexposedExpr" F.int64_t                 
  let declrefexpr                           = F.constant "CXCursor_DeclRefExpr" F.int64_t                   
  let memberrefexpr                         = F.constant "CXCursor_MemberRefExpr" F.int64_t                 
  let callexpr                              = F.constant "CXCursor_CallExpr" F.int64_t                      
  let objcmessageexpr                       = F.constant "CXCursor_ObjCMessageExpr" F.int64_t               
  let blockexpr                             = F.constant "CXCursor_BlockExpr" F.int64_t                     
  let integerliteral                        = F.constant "CXCursor_IntegerLiteral" F.int64_t                
  let floatingliteral                       = F.constant "CXCursor_FloatingLiteral" F.int64_t               
  let imaginaryliteral                      = F.constant "CXCursor_ImaginaryLiteral" F.int64_t              
  let stringliteral                         = F.constant "CXCursor_StringLiteral" F.int64_t                 
  let characterliteral                      = F.constant "CXCursor_CharacterLiteral" F.int64_t              
  let parenexpr                             = F.constant "CXCursor_ParenExpr" F.int64_t                     
  let unaryoperator                         = F.constant "CXCursor_UnaryOperator" F.int64_t                 
  let arraysubscriptexpr                    = F.constant "CXCursor_ArraySubscriptExpr" F.int64_t            
  let binaryoperator                        = F.constant "CXCursor_BinaryOperator" F.int64_t                
  let compoundassignoperator                = F.constant "CXCursor_CompoundAssignOperator" F.int64_t        
  let conditionaloperator                   = F.constant "CXCursor_ConditionalOperator" F.int64_t           
  let cstylecastexpr                        = F.constant "CXCursor_CStyleCastExpr" F.int64_t                
  let compoundliteralexpr                   = F.constant "CXCursor_CompoundLiteralExpr" F.int64_t           
  let initlistexpr                          = F.constant "CXCursor_InitListExpr" F.int64_t                  
  let addrlabelexpr                         = F.constant "CXCursor_AddrLabelExpr" F.int64_t                 
  let stmtexpr                              = F.constant "CXCursor_StmtExpr" F.int64_t                      
  let genericselectionexpr                  = F.constant "CXCursor_GenericSelectionExpr" F.int64_t          
  let gnunullexpr                           = F.constant "CXCursor_GNUNullExpr" F.int64_t                   
  let cxxstaticcastexpr                     = F.constant "CXCursor_CXXStaticCastExpr" F.int64_t             
  let cxxdynamiccastexpr                    = F.constant "CXCursor_CXXDynamicCastExpr" F.int64_t            
  let cxxreinterpretcastexpr                = F.constant "CXCursor_CXXReinterpretCastExpr" F.int64_t        
  let cxxconstcastexpr                      = F.constant "CXCursor_CXXConstCastExpr" F.int64_t              
  let cxxfunctionalcastexpr                 = F.constant "CXCursor_CXXFunctionalCastExpr" F.int64_t         
  let cxxtypeidexpr                         = F.constant "CXCursor_CXXTypeidExpr" F.int64_t                 
  let cxxboolliteralexpr                    = F.constant "CXCursor_CXXBoolLiteralExpr" F.int64_t            
  let cxxnullptrliteralexpr                 = F.constant "CXCursor_CXXNullPtrLiteralExpr" F.int64_t         
  let cxxthisexpr                           = F.constant "CXCursor_CXXThisExpr" F.int64_t                   
  let cxxthrowexpr                          = F.constant "CXCursor_CXXThrowExpr" F.int64_t                  
  let cxxnewexpr                            = F.constant "CXCursor_CXXNewExpr" F.int64_t                    
  let cxxdeleteexpr                         = F.constant "CXCursor_CXXDeleteExpr" F.int64_t                 
  let unaryexpr                             = F.constant "CXCursor_UnaryExpr" F.int64_t                     
  let objcstringliteral                     = F.constant "CXCursor_ObjCStringLiteral" F.int64_t             
  let objcencodeexpr                        = F.constant "CXCursor_ObjCEncodeExpr" F.int64_t                
  let objcselectorexpr                      = F.constant "CXCursor_ObjCSelectorExpr" F.int64_t              
  let objcprotocolexpr                      = F.constant "CXCursor_ObjCProtocolExpr" F.int64_t              
  let objcbridgedcastexpr                   = F.constant "CXCursor_ObjCBridgedCastExpr" F.int64_t           
  let packexpansionexpr                     = F.constant "CXCursor_PackExpansionExpr" F.int64_t             
  let sizeofpackexpr                        = F.constant "CXCursor_SizeOfPackExpr" F.int64_t                
  let lambdaexpr                            = F.constant "CXCursor_LambdaExpr" F.int64_t                    
  let objcboolliteralexpr                   = F.constant "CXCursor_ObjCBoolLiteralExpr" F.int64_t           
  let objcselfexpr                          = F.constant "CXCursor_ObjCSelfExpr" F.int64_t                  
  let omparraysectionexpr                   = F.constant "CXCursor_OMPArraySectionExpr" F.int64_t           
  let objcavailabilitycheckexpr             = F.constant "CXCursor_ObjCAvailabilityCheckExpr" F.int64_t     
  let lastexpr                              = F.constant "CXCursor_LastExpr" F.int64_t                      
  let firststmt                             = F.constant "CXCursor_FirstStmt" F.int64_t                     
  let unexposedstmt                         = F.constant "CXCursor_UnexposedStmt" F.int64_t                 
  let labelstmt                             = F.constant "CXCursor_LabelStmt" F.int64_t                     
  let compoundstmt                          = F.constant "CXCursor_CompoundStmt" F.int64_t                  
  let casestmt                              = F.constant "CXCursor_CaseStmt" F.int64_t                      
  let defaultstmt                           = F.constant "CXCursor_DefaultStmt" F.int64_t                   
  let ifstmt                                = F.constant "CXCursor_IfStmt" F.int64_t                        
  let switchstmt                            = F.constant "CXCursor_SwitchStmt" F.int64_t                    
  let whilestmt                             = F.constant "CXCursor_WhileStmt" F.int64_t                     
  let dostmt                                = F.constant "CXCursor_DoStmt" F.int64_t                        
  let forstmt                               = F.constant "CXCursor_ForStmt" F.int64_t                       
  let gotostmt                              = F.constant "CXCursor_GotoStmt" F.int64_t                      
  let indirectgotostmt                      = F.constant "CXCursor_IndirectGotoStmt" F.int64_t              
  let continuestmt                          = F.constant "CXCursor_ContinueStmt" F.int64_t                  
  let breakstmt                             = F.constant "CXCursor_BreakStmt" F.int64_t                     
  let returnstmt                            = F.constant "CXCursor_ReturnStmt" F.int64_t                    
  let gccasmstmt                            = F.constant "CXCursor_GCCAsmStmt" F.int64_t                    
  let asmstmt                               = F.constant "CXCursor_AsmStmt" F.int64_t                       
  let objcattrystmt                         = F.constant "CXCursor_ObjCAtTryStmt" F.int64_t                 
  let objcatcatchstmt                       = F.constant "CXCursor_ObjCAtCatchStmt" F.int64_t               
  let objcatfinallystmt                     = F.constant "CXCursor_ObjCAtFinallyStmt" F.int64_t             
  let objcatthrowstmt                       = F.constant "CXCursor_ObjCAtThrowStmt" F.int64_t               
  let objcatsynchronizedstmt                = F.constant "CXCursor_ObjCAtSynchronizedStmt" F.int64_t        
  let objcautoreleasepoolstmt               = F.constant "CXCursor_ObjCAutoreleasePoolStmt" F.int64_t       
  let objcforcollectionstmt                 = F.constant "CXCursor_ObjCForCollectionStmt" F.int64_t         
  let cxxcatchstmt                          = F.constant "CXCursor_CXXCatchStmt" F.int64_t                  
  let cxxtrystmt                            = F.constant "CXCursor_CXXTryStmt" F.int64_t                    
  let cxxforrangestmt                       = F.constant "CXCursor_CXXForRangeStmt" F.int64_t               
  let sehtrystmt                            = F.constant "CXCursor_SEHTryStmt" F.int64_t                    
  let sehexceptstmt                         = F.constant "CXCursor_SEHExceptStmt" F.int64_t                 
  let sehfinallystmt                        = F.constant "CXCursor_SEHFinallyStmt" F.int64_t                
  let msasmstmt                             = F.constant "CXCursor_MSAsmStmt" F.int64_t                     
  let nullstmt                              = F.constant "CXCursor_NullStmt" F.int64_t                      
  let declstmt                              = F.constant "CXCursor_DeclStmt" F.int64_t                      
  let ompparalleldirective                  = F.constant "CXCursor_OMPParallelDirective" F.int64_t          
  let ompsimddirective                      = F.constant "CXCursor_OMPSimdDirective" F.int64_t              
  let ompfordirective                       = F.constant "CXCursor_OMPForDirective" F.int64_t               
  let ompsectionsdirective                  = F.constant "CXCursor_OMPSectionsDirective" F.int64_t          
  let ompsectiondirective                   = F.constant "CXCursor_OMPSectionDirective" F.int64_t           
  let ompsingledirective                    = F.constant "CXCursor_OMPSingleDirective" F.int64_t            
  let ompparallelfordirective               = F.constant "CXCursor_OMPParallelForDirective" F.int64_t       
  let ompparallelsectionsdirective          = F.constant "CXCursor_OMPParallelSectionsDirective" F.int64_t  
  let omptaskdirective                      = F.constant "CXCursor_OMPTaskDirective" F.int64_t              
  let ompmasterdirective                    = F.constant "CXCursor_OMPMasterDirective" F.int64_t            
  let ompcriticaldirective                  = F.constant "CXCursor_OMPCriticalDirective" F.int64_t          
  let omptaskyielddirective                 = F.constant "CXCursor_OMPTaskyieldDirective" F.int64_t         
  let ompbarrierdirective                   = F.constant "CXCursor_OMPBarrierDirective" F.int64_t           
  let omptaskwaitdirective                  = F.constant "CXCursor_OMPTaskwaitDirective" F.int64_t          
  let ompflushdirective                     = F.constant "CXCursor_OMPFlushDirective" F.int64_t             
  let sehleavestmt                          = F.constant "CXCursor_SEHLeaveStmt" F.int64_t                  
  let ompordereddirective                   = F.constant "CXCursor_OMPOrderedDirective" F.int64_t           
  let ompatomicdirective                    = F.constant "CXCursor_OMPAtomicDirective" F.int64_t            
  let ompforsimddirective                   = F.constant "CXCursor_OMPForSimdDirective" F.int64_t           
  let ompparallelforsimddirective           = F.constant "CXCursor_OMPParallelForSimdDirective" F.int64_t   
  let omptargetdirective                    = F.constant "CXCursor_OMPTargetDirective" F.int64_t            
  let ompteamsdirective                     = F.constant "CXCursor_OMPTeamsDirective" F.int64_t             
  let omptaskgroupdirective                 = F.constant "CXCursor_OMPTaskgroupDirective" F.int64_t         
  let ompcancellationpointdirective         = F.constant "CXCursor_OMPCancellationPointDirective" F.int64_t 
  let ompcanceldirective                    = F.constant "CXCursor_OMPCancelDirective" F.int64_t            
  let omptargetdatadirective                = F.constant "CXCursor_OMPTargetDataDirective" F.int64_t        
  let omptaskloopdirective                  = F.constant "CXCursor_OMPTaskLoopDirective" F.int64_t          
  let omptaskloopsimddirective              = F.constant "CXCursor_OMPTaskLoopSimdDirective" F.int64_t      
  let ompdistributedirective                = F.constant "CXCursor_OMPDistributeDirective" F.int64_t        
  let omptargetenterdatadirective           = F.constant "CXCursor_OMPTargetEnterDataDirective" F.int64_t   
  let omptargetexitdatadirective            = F.constant "CXCursor_OMPTargetExitDataDirective" F.int64_t    
  let omptargetparalleldirective            = F.constant "CXCursor_OMPTargetParallelDirective" F.int64_t    
  let omptargetparallelfordirective         = F.constant "CXCursor_OMPTargetParallelForDirective" F.int64_t 
  let omptargetupdatedirective              = F.constant "CXCursor_OMPTargetUpdateDirective" F.int64_t      
  let ompdistributeparallelfordirective     = F.constant "CXCursor_OMPDistributeParallelForDirective" F.int64_t 
  let ompdistributeparallelforsimddirective = F.constant "CXCursor_OMPDistributeParallelForSimdDirective" F.int64_t 
  let ompdistributesimddirective            = F.constant "CXCursor_OMPDistributeSimdDirective" F.int64_t 
  let omptargetparallelforsimddirective     = F.constant "CXCursor_OMPTargetParallelForSimdDirective" F.int64_t 
  let laststmt                              = F.constant "CXCursor_LastStmt" F.int64_t 
  let translationunit                       = F.constant "CXCursor_TranslationUnit" F.int64_t               
  let firstattr                             = F.constant "CXCursor_FirstAttr" F.int64_t                     
  let unexposedattr                         = F.constant "CXCursor_UnexposedAttr" F.int64_t                 
  let ibactionattr                          = F.constant "CXCursor_IBActionAttr" F.int64_t                  
  let iboutletattr                          = F.constant "CXCursor_IBOutletAttr" F.int64_t                  
  let iboutletcollectionattr                = F.constant "CXCursor_IBOutletCollectionAttr" F.int64_t        
  let cxxfinalattr                          = F.constant "CXCursor_CXXFinalAttr" F.int64_t                  
  let cxxoverrideattr                       = F.constant "CXCursor_CXXOverrideAttr" F.int64_t               
  let annotateattr                          = F.constant "CXCursor_AnnotateAttr" F.int64_t                  
  let asmlabelattr                          = F.constant "CXCursor_AsmLabelAttr" F.int64_t                  
  let packedattr                            = F.constant "CXCursor_PackedAttr" F.int64_t                    
  let pureattr                              = F.constant "CXCursor_PureAttr" F.int64_t                      
  let constattr                             = F.constant "CXCursor_ConstAttr" F.int64_t                     
  let noduplicateattr                       = F.constant "CXCursor_NoDuplicateAttr" F.int64_t               
  let cudaconstantattr                      = F.constant "CXCursor_CUDAConstantAttr" F.int64_t              
  let cudadeviceattr                        = F.constant "CXCursor_CUDADeviceAttr" F.int64_t                
  let cudaglobalattr                        = F.constant "CXCursor_CUDAGlobalAttr" F.int64_t                
  let cudahostattr                          = F.constant "CXCursor_CUDAHostAttr" F.int64_t                  
  let cudasharedattr                        = F.constant "CXCursor_CUDASharedAttr" F.int64_t                
  let visibilityattr                        = F.constant "CXCursor_VisibilityAttr" F.int64_t                
  let dllexport                             = F.constant "CXCursor_DLLExport" F.int64_t                     
  let dllimport                             = F.constant "CXCursor_DLLImport" F.int64_t                     
  let lastattr                              = F.constant "CXCursor_LastAttr" F.int64_t                      
  let preprocessingdirective                = F.constant "CXCursor_PreprocessingDirective" F.int64_t        
  let macrodefinition                       = F.constant "CXCursor_MacroDefinition" F.int64_t               
  let macroexpansion                        = F.constant "CXCursor_MacroExpansion" F.int64_t                
  let macroinstantiation                    = F.constant "CXCursor_MacroInstantiation" F.int64_t            
  let inclusiondirective                    = F.constant "CXCursor_InclusionDirective" F.int64_t            
  let firstpreprocessing                    = F.constant "CXCursor_FirstPreprocessing" F.int64_t            
  let lastpreprocessing                     = F.constant "CXCursor_LastPreprocessing" F.int64_t             
  let moduleimportdecl                      = F.constant "CXCursor_ModuleImportDecl" F.int64_t              
  let typealiastemplatedecl                 = F.constant "CXCursor_TypeAliasTemplateDecl" F.int64_t         
  let staticassert                          = F.constant "CXCursor_StaticAssert" F.int64_t                  
  let firstextradecl                        = F.constant "CXCursor_FirstExtraDecl" F.int64_t                
  let lastextradecl                         = F.constant "CXCursor_LastExtraDecl" F.int64_t                 
  let overloadcandidate                     = F.constant "CXCursor_OverloadCandidate" F.int64_t             



  let cursor_kind = F.enum "CXCursorKind" [
      UnexposedDecl, unexposeddecl;
      StructDecl, structdecl;
      UnionDecl, uniondecl;
      ClassDecl, classdecl;
      EnumDecl, enumdecl;
      FieldDecl, fielddecl;
      EnumConstantDecl, enumconstantdecl;
      FunctionDecl, functiondecl;
      VarDecl, vardecl;
      ParmDecl, parmdecl;
      ObjCInterfaceDecl, objcinterfacedecl;
      ObjCCategoryDecl, objccategorydecl;
      ObjCProtocolDecl, objcprotocoldecl;
      ObjCPropertyDecl, objcpropertydecl;
      ObjCIvarDecl, objcivardecl;
      ObjCInstanceMethodDecl, objcinstancemethoddecl;
      ObjCClassMethodDecl, objcclassmethoddecl;
      ObjCImplementationDecl, objcimplementationdecl;
      ObjCCategoryImplDecl, objccategoryimpldecl;
      TypedefDecl, typedefdecl;
      CXXMethod, cxxmethod;
      Namespace, namespace;
      LinkageSpec, linkagespec;
      Constructor, constructor;
      Destructor, destructor;
      ConversionFunction, conversionfunction;
      TemplateTypeParameter, templatetypeparameter;
      NonTypeTemplateParameter, nontypetemplateparameter;
      TemplateTemplateParameter, templatetemplateparameter;
      FunctionTemplate, functiontemplate;
      ClassTemplate, classtemplate;
      ClassTemplatePartialSpecialization, classtemplatepartialspecialization;
      NamespaceAlias, namespacealias;
      UsingDirective, usingdirective;
      UsingDeclaration, usingdeclaration;
      TypeAliasDecl, typealiasdecl;
      ObjCSynthesizeDecl, objcsynthesizedecl;
      ObjCDynamicDecl, objcdynamicdecl;
      CXXAccessSpecifier, cxxaccessspecifier;
      FirstDecl, firstdecl;
      LastDecl, lastdecl;
      FirstRef, firstref;
      ObjCSuperClassRef, objcsuperclassref;
      ObjCProtocolRef, objcprotocolref;
      ObjCClassRef, objcclassref;
      TypeRef, typeref;
      CXXBaseSpecifier, cxxbasespecifier;
      TemplateRef, templateref;
      NamespaceRef, namespaceref;
      MemberRef, memberref;
      LabelRef, labelref;
      OverloadedDeclRef, overloadeddeclref;
      VariableRef, variableref;
      LastRef, lastref;
      FirstInvalid, firstinvalid;
      InvalidFile, invalidfile;
      NoDeclFound, nodeclfound;
      NotImplemented, notimplemented;
      InvalidCode, invalidcode;
      LastInvalid, lastinvalid;
      FirstExpr, firstexpr;
      UnexposedExpr, unexposedexpr;
      DeclRefExpr, declrefexpr;
      MemberRefExpr, memberrefexpr;
      CallExpr, callexpr;
      ObjCMessageExpr, objcmessageexpr;
      BlockExpr, blockexpr;
      IntegerLiteral, integerliteral;
      FloatingLiteral, floatingliteral;
      ImaginaryLiteral, imaginaryliteral;
      StringLiteral, stringliteral;
      CharacterLiteral, characterliteral;
      ParenExpr, parenexpr;
      UnaryOperator, unaryoperator;
      ArraySubscriptExpr, arraysubscriptexpr;
      BinaryOperator, binaryoperator;
      CompoundAssignOperator, compoundassignoperator;
      ConditionalOperator, conditionaloperator;
      CStyleCastExpr, cstylecastexpr;
      CompoundLiteralExpr, compoundliteralexpr;
      InitListExpr, initlistexpr;
      AddrLabelExpr, addrlabelexpr;
      StmtExpr, stmtexpr;
      GenericSelectionExpr, genericselectionexpr;
      GNUNullExpr, gnunullexpr;
      CXXStaticCastExpr, cxxstaticcastexpr;
      CXXDynamicCastExpr, cxxdynamiccastexpr;
      CXXReinterpretCastExpr, cxxreinterpretcastexpr;
      CXXConstCastExpr, cxxconstcastexpr;
      CXXFunctionalCastExpr, cxxfunctionalcastexpr;
      CXXTypeidExpr, cxxtypeidexpr;
      CXXBoolLiteralExpr, cxxboolliteralexpr;
      CXXNullPtrLiteralExpr, cxxnullptrliteralexpr;
      CXXThisExpr, cxxthisexpr;
      CXXThrowExpr, cxxthrowexpr;
      CXXNewExpr, cxxnewexpr;
      CXXDeleteExpr, cxxdeleteexpr;
      UnaryExpr, unaryexpr;
      ObjCStringLiteral, objcstringliteral;
      ObjCEncodeExpr, objcencodeexpr;
      ObjCSelectorExpr, objcselectorexpr;
      ObjCProtocolExpr, objcprotocolexpr;
      ObjCBridgedCastExpr, objcbridgedcastexpr;
      PackExpansionExpr, packexpansionexpr;
      SizeOfPackExpr, sizeofpackexpr;
      LambdaExpr, lambdaexpr;
      ObjCBoolLiteralExpr, objcboolliteralexpr;
      ObjCSelfExpr, objcselfexpr;
      OMPArraySectionExpr, omparraysectionexpr;
      ObjCAvailabilityCheckExpr, objcavailabilitycheckexpr;
      LastExpr, lastexpr;
      FirstStmt, firststmt;
      UnexposedStmt, unexposedstmt;
      LabelStmt, labelstmt;
      CompoundStmt, compoundstmt;
      CaseStmt, casestmt;
      DefaultStmt, defaultstmt;
      IfStmt, ifstmt;
      SwitchStmt, switchstmt;
      WhileStmt, whilestmt;
      DoStmt, dostmt;
      ForStmt, forstmt;
      GotoStmt, gotostmt;
      IndirectGotoStmt, indirectgotostmt;
      ContinueStmt, continuestmt;
      BreakStmt, breakstmt;
      ReturnStmt, returnstmt;
      GCCAsmStmt, gccasmstmt;
      AsmStmt, asmstmt;
      ObjCAtTryStmt, objcattrystmt;
      ObjCAtCatchStmt, objcatcatchstmt;
      ObjCAtFinallyStmt, objcatfinallystmt;
      ObjCAtThrowStmt, objcatthrowstmt;
      ObjCAtSynchronizedStmt, objcatsynchronizedstmt;
      ObjCAutoreleasePoolStmt, objcautoreleasepoolstmt;
      ObjCForCollectionStmt, objcforcollectionstmt;
      CXXCatchStmt, cxxcatchstmt;
      CXXTryStmt, cxxtrystmt;
      CXXForRangeStmt, cxxforrangestmt;
      SEHTryStmt, sehtrystmt;
      SEHExceptStmt, sehexceptstmt;
      SEHFinallyStmt, sehfinallystmt;
      MSAsmStmt, msasmstmt;
      NullStmt, nullstmt;
      DeclStmt, declstmt;
      OMPParallelDirective, ompparalleldirective;
      OMPSimdDirective, ompsimddirective;
      OMPForDirective, ompfordirective;
      OMPSectionsDirective, ompsectionsdirective;
      OMPSectionDirective, ompsectiondirective;
      OMPSingleDirective, ompsingledirective;
      OMPParallelForDirective, ompparallelfordirective;
      OMPParallelSectionsDirective, ompparallelsectionsdirective;
      OMPTaskDirective, omptaskdirective;
      OMPMasterDirective, ompmasterdirective;
      OMPCriticalDirective, ompcriticaldirective;
      OMPTaskyieldDirective, omptaskyielddirective;
      OMPBarrierDirective, ompbarrierdirective;
      OMPTaskwaitDirective, omptaskwaitdirective;
      OMPFlushDirective, ompflushdirective;
      SEHLeaveStmt, sehleavestmt;
      OMPOrderedDirective, ompordereddirective;
      OMPAtomicDirective, ompatomicdirective;
      OMPForSimdDirective, ompforsimddirective;
      OMPParallelForSimdDirective, ompparallelforsimddirective;
      OMPTargetDirective, omptargetdirective;
      OMPTeamsDirective, ompteamsdirective;
      OMPTaskgroupDirective, omptaskgroupdirective;
      OMPCancellationPointDirective, ompcancellationpointdirective;
      OMPCancelDirective, ompcanceldirective;
      OMPTargetDataDirective, omptargetdatadirective;
      OMPTaskLoopDirective, omptaskloopdirective;
      OMPTaskLoopSimdDirective, omptaskloopsimddirective;
      OMPDistributeDirective, ompdistributedirective;
      OMPTargetEnterDataDirective, omptargetenterdatadirective;
      OMPTargetExitDataDirective, omptargetexitdatadirective;
      OMPTargetParallelDirective, omptargetparalleldirective;
      OMPTargetParallelForDirective, omptargetparallelfordirective;
      OMPTargetUpdateDirective, omptargetupdatedirective;
      OMPDistributeParallelForDirective, ompdistributeparallelfordirective;
      OMPDistributeParallelForSimdDirective, ompdistributeparallelforsimddirective;
      OMPDistributeSimdDirective, ompdistributesimddirective;
      OMPTargetParallelForSimdDirective, omptargetparallelforsimddirective;
      LastStmt, laststmt;
      TranslationUnit, translationunit;
      FirstAttr, firstattr;
      UnexposedAttr, unexposedattr;
      IBActionAttr, ibactionattr;
      IBOutletAttr, iboutletattr;
      IBOutletCollectionAttr, iboutletcollectionattr;
      CXXFinalAttr, cxxfinalattr;
      CXXOverrideAttr, cxxoverrideattr;
      AnnotateAttr, annotateattr;
      AsmLabelAttr, asmlabelattr;
      PackedAttr, packedattr;
      PureAttr, pureattr;
      ConstAttr, constattr;
      NoDuplicateAttr, noduplicateattr;
      CUDAConstantAttr, cudaconstantattr;
      CUDADeviceAttr, cudadeviceattr;
      CUDAGlobalAttr, cudaglobalattr;
      CUDAHostAttr, cudahostattr;
      CUDASharedAttr, cudasharedattr;
      VisibilityAttr, visibilityattr;
      DLLExport, dllexport;
      DLLImport, dllimport;
      LastAttr, lastattr;
      PreprocessingDirective, preprocessingdirective;
      MacroDefinition, macrodefinition;
      MacroExpansion, macroexpansion;
      MacroInstantiation, macroinstantiation;
      InclusionDirective, inclusiondirective;
      FirstPreprocessing, firstpreprocessing;
      LastPreprocessing, lastpreprocessing;
      ModuleImportDecl, moduleimportdecl;
      TypeAliasTemplateDecl, typealiastemplatedecl;
      StaticAssert, staticassert;
      FirstExtraDecl, firstextradecl;
      LastExtraDecl, lastextradecl;
      OverloadCandidate, overloadcandidate;
                         ]


  type cx_child_visit_result =
    | VisitBreak
    | VisitContinue
    | VisitRecurse

  let visitbreak = F.constant "CXChildVisit_Break" F.int64_t
  let visitcontinue = F.constant "CXChildVisit_Continue" F.int64_t
  let visitrecurse = F.constant "CXChildVisit_Recurse" F.int64_t

  let cx_child_visit_result = F.enum "CXChildVisitResult" [
                                     VisitBreak, visitbreak;
                                     VisitContinue, visitcontinue;
                                     VisitRecurse, visitrecurse;

                                   ]

  type cx_type
  let cx_type : cx_type Ctypes.structure F.typ = F.structure "_CXType"
  let cx_type = F.typedef cx_type "CXType"
  let kind = F.field cx_type "kind" kind
  let d = F.field cx_type "data" (F.array 2 (F.ptr F.void))
  let () = F.seal cx_type
  type cx_cursor

  let cx_cursor : cx_cursor Ctypes.structure F.typ = F.structure "_CXCursor"
  let cx_cursor = F.typedef cx_cursor "CXCursor" 
  let kind = F.field cx_cursor "kind" cursor_kind
  let xdata = F.field cx_cursor "xdata" F.int
  (*let data = F.field cx_cursor "data" (F.array 3 (F.ptr F.void)) *)
  let data = Array.init 3 (fun i -> F.field cx_cursor ("data") (F.ptr F.void))
  let () = F.seal cx_cursor 

  type cx_client_data = unit Ctypes.ptr
  let cx_client_data : cx_client_data F.typ = F.ptr F.void 

  type cx_translation_unit = unit Ctypes.ptr
  let cx_translation_unit : cx_translation_unit F.typ = F.ptr F.void

  type cx_index = unit Ctypes.ptr
  let cx_index : cx_index F.typ = (F.ptr F.void)




end
