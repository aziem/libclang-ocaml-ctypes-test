open Ctypes

module Types (F : Cstubs.Types.TYPE) = 
struct
  open F

  type cx_error_code = 
    | CXError_Success
    | CXError_Failure
    | CXError_Crashed
    | CXError_InvalidArguments
    | CXError_ASTReadError

  let cxerror_success = constant "CXError_Success" int64_t
  let cxerror_failure = constant "CXError_Failure" int64_t
  let cxerror_crashed = constant "CXError_Crashed" int64_t
  let cxerror_invalidarguments = constant "CXError_InvalidArguments" int64_t
  let cxerror_astreaderror = constant "CXError_ASTReadError" int64_t

  let cx_error_code = enum "CXErrorCode" [
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


  let invalid             = constant "CXType_Invalid" int64_t
  let unexposed           = constant "CXType_Unexposed" int64_t
  let cxvoid                = constant "CXType_Void" int64_t
  let bool                = constant "CXType_Bool" int64_t
  let char_u              = constant "CXType_Char_U" int64_t
  let uchar               = constant "CXType_UChar" int64_t
  let char16              = constant "CXType_Char16" int64_t
  let char32              = constant "CXType_Char32" int64_t
  let ushort              = constant "CXType_UShort" int64_t
  let uint                = constant "CXType_UInt" int64_t
  let ulong               = constant "CXType_ULong" int64_t
  let ulonglong           = constant "CXType_ULongLong" int64_t
  let uint128             = constant "CXType_UInt128" int64_t
  let char_s              = constant "CXType_Char_S" int64_t
  let schar               = constant "CXType_SChar" int64_t
  let wchar               = constant "CXType_WChar" int64_t
  let short               = constant "CXType_Short" int64_t
  let int                 = constant "CXType_Int" int64_t
  let long                = constant "CXType_Long" int64_t
  let longlong            = constant "CXType_LongLong" int64_t
  let int128              = constant "CXType_Int128" int64_t
  let float               = constant "CXType_Float" int64_t
  let double              = constant "CXType_Double" int64_t
  let longdouble          = constant "CXType_LongDouble" int64_t
  let nullptr             = constant "CXType_NullPtr" int64_t
  let overload            = constant "CXType_Overload" int64_t
  let dependent           = constant "CXType_Dependent" int64_t
  let objcid              = constant "CXType_ObjCId" int64_t
  let objcclass           = constant "CXType_ObjCClass" int64_t
  let objcsel             = constant "CXType_ObjCSel" int64_t
  let float128            = constant "CXType_Float128" int64_t
  let firstbuiltin        = constant "CXType_FirstBuiltin" int64_t
  let lastbuiltin         = constant "CXType_LastBuiltin"  int64_t
  let complex             = constant "CXType_Complex" int64_t
  let pointer             = constant "CXType_Pointer" int64_t
  let blockpointer        = constant "CXType_BlockPointer" int64_t
  let lvaluereference     = constant "CXType_LValueReference" int64_t
  let rvaluereference     = constant "CXType_RValueReference" int64_t
  let record              = constant "CXType_Record" int64_t
  let cxenum                = constant "CXType_Enum" int64_t
  let typedef             = constant "CXType_Typedef" int64_t
  let objcinterface       = constant "CXType_ObjCInterface" int64_t
  let objcobjectpointer   = constant "CXType_ObjCObjectPointer" int64_t
  let functionnoproto     = constant "CXType_FunctionNoProto" int64_t
  let functionproto       = constant "CXType_FunctionProto" int64_t
  let constantarray       = constant "CXType_ConstantArray" int64_t
  let vector              = constant "CXType_Vector" int64_t
  let incompletearray     = constant "CXType_IncompleteArray" int64_t
  let variablearray       = constant "CXType_VariableArray" int64_t
  let dependentsizedarray = constant "CXType_DependentSizedArray" int64_t
  let memberpointer       = constant "CXType_MemberPointer" int64_t
  let auto                = constant "CXType_Auto" int64_t
  let elaborated          = constant "CXType_Elaborated" int64_t

  let kind = enum "CXTypeKind" [
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

  let cxcallingconv_default = constant "CXCallingConv_Default" int64_t
  let cxcallingconv_c = constant "CXCallingConv_C" int64_t
  let cxcallingconv_x86stdcall = constant "CXCallingConv_X86StdCall" int64_t
  let cxcallingconv_x86fastcall = constant "CXCallingConv_X86FastCall" int64_t
  let cxcallingconv_x86thiscall = constant "CXCallingConv_X86ThisCall" int64_t
  let cxcallingconv_x86pascal = constant "CXCallingConv_X86Pascal" int64_t
  let cxcallingconv_aapcs = constant "CXCallingConv_AAPCS" int64_t
  let cxcallingconv_aapcs_vfp = constant "CXCallingConv_AAPCS_VFP" int64_t
  let cxcallingconv_inteloclbicc = constant "CXCallingConv_IntelOclBicc" int64_t
  let cxcallingconv_x86_64win64 = constant "CXCallingConv_X86_64Win64" int64_t
  let cxcallingconv_x86_64sysv = constant "CXCallingConv_X86_64SysV" int64_t
  let cxcallingconv_x86vectorcall = constant "CXCallingConv_X86VectorCall" int64_t
  let cxcallingconv_swift = constant "CXCallingConv_Swift" int64_t
  let cxcallingconv_preservemost = constant "CXCallingConv_PreserveMost" int64_t
  let cxcallingconv_preserveall = constant "CXCallingConv_PreserveAll" int64_t
  let cxcallingconv_invalid = constant "CXCallingConv_Invalid" int64_t
  let cxcallingconv_unexposed = constant "CXCallingConv_Unexposed" int64_t

  let calling_conv = enum "CXCallingConv" [
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

  let invalidlayout = constant "CXTypeLayoutError_Invalid"  int64_t
  let incompletelayout = constant "CXTypeLayoutError_Incomplete"  int64_t
  let dependentlayout = constant "CXTypeLayoutError_Dependent"  int64_t
  let notconstantsizelayout = constant "CXTypeLayoutError_NotConstantSize"  int64_t
  let invalidfieldnamelayout = constant "CXTypeLayoutError_InvalidFieldName"  int64_t

  let layout_error = enum "CXTypeLayoutError" [
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

  let nooptions = constant "CXTranslationUnit_None" int64_t
  let detailedpreprocessingrecord = constant "CXTranslationUnit_DetailedPreprocessingRecord" int64_t
  let incomplete = constant "CXTranslationUnit_Incomplete" int64_t
  let precompiledpreamble = constant "CXTranslationUnit_PrecompiledPreamble" int64_t
  let cachecompletionresults = constant "CXTranslationUnit_CacheCompletionResults" int64_t
  let forserialization = constant "CXTranslationUnit_ForSerialization" int64_t
  let cxxchainedpch = constant "CXTranslationUnit_CXXChainedPCH" int64_t
  let skipfunctionbodies = constant "CXTranslationUnit_SkipFunctionBodies" int64_t
  let includebriefcommentsincodecompletion = constant "CXTranslationUnit_IncludeBriefCommentsInCodeCompletion" int64_t
  let createpreambleonfirstparse = constant "CXTranslationUnit_CreatePreambleOnFirstParse" int64_t
  let keepgoing = constant "CXTranslationUnit_KeepGoing" int64_t

  let translation_unit_options = enum "CXTranslationUnit_Flags"
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

  let unexposeddecl                         = constant "CXCursor_UnexposedDecl" int64_t                 
  let structdecl                            = constant "CXCursor_StructDecl" int64_t                    
  let uniondecl                             = constant "CXCursor_UnionDecl" int64_t                     
  let classdecl                             = constant "CXCursor_ClassDecl" int64_t                     
  let enumdecl                              = constant "CXCursor_EnumDecl" int64_t                      
  let fielddecl                             = constant "CXCursor_FieldDecl" int64_t                     
  let enumconstantdecl                      = constant "CXCursor_EnumConstantDecl" int64_t              
  let functiondecl                          = constant "CXCursor_FunctionDecl" int64_t                  
  let vardecl                               = constant "CXCursor_VarDecl" int64_t                       
  let parmdecl                              = constant "CXCursor_ParmDecl" int64_t                      
  let objcinterfacedecl                     = constant "CXCursor_ObjCInterfaceDecl" int64_t             
  let objccategorydecl                      = constant "CXCursor_ObjCCategoryDecl" int64_t              
  let objcprotocoldecl                      = constant "CXCursor_ObjCProtocolDecl" int64_t              
  let objcpropertydecl                      = constant "CXCursor_ObjCPropertyDecl" int64_t              
  let objcivardecl                          = constant "CXCursor_ObjCIvarDecl" int64_t                  
  let objcinstancemethoddecl                = constant "CXCursor_ObjCInstanceMethodDecl" int64_t        
  let objcclassmethoddecl                   = constant "CXCursor_ObjCClassMethodDecl" int64_t           
  let objcimplementationdecl                = constant "CXCursor_ObjCImplementationDecl" int64_t        
  let objccategoryimpldecl                  = constant "CXCursor_ObjCCategoryImplDecl" int64_t          
  let typedefdecl                           = constant "CXCursor_TypedefDecl" int64_t                   
  let cxxmethod                             = constant "CXCursor_CXXMethod" int64_t                     
  let namespace                             = constant "CXCursor_Namespace" int64_t                     
  let linkagespec                           = constant "CXCursor_LinkageSpec" int64_t                   
  let constructor                           = constant "CXCursor_Constructor" int64_t                   
  let destructor                            = constant "CXCursor_Destructor" int64_t                    
  let conversionfunction                    = constant "CXCursor_ConversionFunction" int64_t            
  let templatetypeparameter                 = constant "CXCursor_TemplateTypeParameter" int64_t         
  let nontypetemplateparameter              = constant "CXCursor_NonTypeTemplateParameter" int64_t      
  let templatetemplateparameter             = constant "CXCursor_TemplateTemplateParameter" int64_t     
  let functiontemplate                      = constant "CXCursor_FunctionTemplate" int64_t              
  let classtemplate                         = constant "CXCursor_ClassTemplate" int64_t                 
  let classtemplatepartialspecialization    = constant "CXCursor_ClassTemplatePartialSpecialization" int64_t 
  let namespacealias                        = constant "CXCursor_NamespaceAlias" int64_t                
  let usingdirective                        = constant "CXCursor_UsingDirective" int64_t                
  let usingdeclaration                      = constant "CXCursor_UsingDeclaration" int64_t              
  let typealiasdecl                         = constant "CXCursor_TypeAliasDecl" int64_t                 
  let objcsynthesizedecl                    = constant "CXCursor_ObjCSynthesizeDecl" int64_t            
  let objcdynamicdecl                       = constant "CXCursor_ObjCDynamicDecl" int64_t               
  let cxxaccessspecifier                    = constant "CXCursor_CXXAccessSpecifier" int64_t            
  let firstdecl                             = constant "CXCursor_FirstDecl" int64_t                     
  let lastdecl                              = constant "CXCursor_LastDecl" int64_t                      
  let firstref                              = constant "CXCursor_FirstRef" int64_t                      
  let objcsuperclassref                     = constant "CXCursor_ObjCSuperClassRef" int64_t             
  let objcprotocolref                       = constant "CXCursor_ObjCProtocolRef" int64_t               
  let objcclassref                          = constant "CXCursor_ObjCClassRef" int64_t                  
  let typeref                               = constant "CXCursor_TypeRef" int64_t                       
  let cxxbasespecifier                      = constant "CXCursor_CXXBaseSpecifier" int64_t              
  let templateref                           = constant "CXCursor_TemplateRef" int64_t                   
  let namespaceref                          = constant "CXCursor_NamespaceRef" int64_t                  
  let memberref                             = constant "CXCursor_MemberRef" int64_t                     
  let labelref                              = constant "CXCursor_LabelRef" int64_t                      
  let overloadeddeclref                     = constant "CXCursor_OverloadedDeclRef" int64_t             
  let variableref                           = constant "CXCursor_VariableRef" int64_t                   
  let lastref                               = constant "CXCursor_LastRef" int64_t                       
  let firstinvalid                          = constant "CXCursor_FirstInvalid" int64_t                  
  let invalidfile                           = constant "CXCursor_InvalidFile" int64_t                   
  let nodeclfound                           = constant "CXCursor_NoDeclFound" int64_t                   
  let notimplemented                        = constant "CXCursor_NotImplemented" int64_t                
  let invalidcode                           = constant "CXCursor_InvalidCode" int64_t                   
  let lastinvalid                           = constant "CXCursor_LastInvalid" int64_t                   
  let firstexpr                             = constant "CXCursor_FirstExpr" int64_t                     
  let unexposedexpr                         = constant "CXCursor_UnexposedExpr" int64_t                 
  let declrefexpr                           = constant "CXCursor_DeclRefExpr" int64_t                   
  let memberrefexpr                         = constant "CXCursor_MemberRefExpr" int64_t                 
  let callexpr                              = constant "CXCursor_CallExpr" int64_t                      
  let objcmessageexpr                       = constant "CXCursor_ObjCMessageExpr" int64_t               
  let blockexpr                             = constant "CXCursor_BlockExpr" int64_t                     
  let integerliteral                        = constant "CXCursor_IntegerLiteral" int64_t                
  let floatingliteral                       = constant "CXCursor_FloatingLiteral" int64_t               
  let imaginaryliteral                      = constant "CXCursor_ImaginaryLiteral" int64_t              
  let stringliteral                         = constant "CXCursor_StringLiteral" int64_t                 
  let characterliteral                      = constant "CXCursor_CharacterLiteral" int64_t              
  let parenexpr                             = constant "CXCursor_ParenExpr" int64_t                     
  let unaryoperator                         = constant "CXCursor_UnaryOperator" int64_t                 
  let arraysubscriptexpr                    = constant "CXCursor_ArraySubscriptExpr" int64_t            
  let binaryoperator                        = constant "CXCursor_BinaryOperator" int64_t                
  let compoundassignoperator                = constant "CXCursor_CompoundAssignOperator" int64_t        
  let conditionaloperator                   = constant "CXCursor_ConditionalOperator" int64_t           
  let cstylecastexpr                        = constant "CXCursor_CStyleCastExpr" int64_t                
  let compoundliteralexpr                   = constant "CXCursor_CompoundLiteralExpr" int64_t           
  let initlistexpr                          = constant "CXCursor_InitListExpr" int64_t                  
  let addrlabelexpr                         = constant "CXCursor_AddrLabelExpr" int64_t                 
  let stmtexpr                              = constant "CXCursor_StmtExpr" int64_t                      
  let genericselectionexpr                  = constant "CXCursor_GenericSelectionExpr" int64_t          
  let gnunullexpr                           = constant "CXCursor_GNUNullExpr" int64_t                   
  let cxxstaticcastexpr                     = constant "CXCursor_CXXStaticCastExpr" int64_t             
  let cxxdynamiccastexpr                    = constant "CXCursor_CXXDynamicCastExpr" int64_t            
  let cxxreinterpretcastexpr                = constant "CXCursor_CXXReinterpretCastExpr" int64_t        
  let cxxconstcastexpr                      = constant "CXCursor_CXXConstCastExpr" int64_t              
  let cxxfunctionalcastexpr                 = constant "CXCursor_CXXFunctionalCastExpr" int64_t         
  let cxxtypeidexpr                         = constant "CXCursor_CXXTypeidExpr" int64_t                 
  let cxxboolliteralexpr                    = constant "CXCursor_CXXBoolLiteralExpr" int64_t            
  let cxxnullptrliteralexpr                 = constant "CXCursor_CXXNullPtrLiteralExpr" int64_t         
  let cxxthisexpr                           = constant "CXCursor_CXXThisExpr" int64_t                   
  let cxxthrowexpr                          = constant "CXCursor_CXXThrowExpr" int64_t                  
  let cxxnewexpr                            = constant "CXCursor_CXXNewExpr" int64_t                    
  let cxxdeleteexpr                         = constant "CXCursor_CXXDeleteExpr" int64_t                 
  let unaryexpr                             = constant "CXCursor_UnaryExpr" int64_t                     
  let objcstringliteral                     = constant "CXCursor_ObjCStringLiteral" int64_t             
  let objcencodeexpr                        = constant "CXCursor_ObjCEncodeExpr" int64_t                
  let objcselectorexpr                      = constant "CXCursor_ObjCSelectorExpr" int64_t              
  let objcprotocolexpr                      = constant "CXCursor_ObjCProtocolExpr" int64_t              
  let objcbridgedcastexpr                   = constant "CXCursor_ObjCBridgedCastExpr" int64_t           
  let packexpansionexpr                     = constant "CXCursor_PackExpansionExpr" int64_t             
  let sizeofpackexpr                        = constant "CXCursor_SizeOfPackExpr" int64_t                
  let lambdaexpr                            = constant "CXCursor_LambdaExpr" int64_t                    
  let objcboolliteralexpr                   = constant "CXCursor_ObjCBoolLiteralExpr" int64_t           
  let objcselfexpr                          = constant "CXCursor_ObjCSelfExpr" int64_t                  
  let omparraysectionexpr                   = constant "CXCursor_OMPArraySectionExpr" int64_t           
  let objcavailabilitycheckexpr             = constant "CXCursor_ObjCAvailabilityCheckExpr" int64_t     
  let lastexpr                              = constant "CXCursor_LastExpr" int64_t                      
  let firststmt                             = constant "CXCursor_FirstStmt" int64_t                     
  let unexposedstmt                         = constant "CXCursor_UnexposedStmt" int64_t                 
  let labelstmt                             = constant "CXCursor_LabelStmt" int64_t                     
  let compoundstmt                          = constant "CXCursor_CompoundStmt" int64_t                  
  let casestmt                              = constant "CXCursor_CaseStmt" int64_t                      
  let defaultstmt                           = constant "CXCursor_DefaultStmt" int64_t                   
  let ifstmt                                = constant "CXCursor_IfStmt" int64_t                        
  let switchstmt                            = constant "CXCursor_SwitchStmt" int64_t                    
  let whilestmt                             = constant "CXCursor_WhileStmt" int64_t                     
  let dostmt                                = constant "CXCursor_DoStmt" int64_t                        
  let forstmt                               = constant "CXCursor_ForStmt" int64_t                       
  let gotostmt                              = constant "CXCursor_GotoStmt" int64_t                      
  let indirectgotostmt                      = constant "CXCursor_IndirectGotoStmt" int64_t              
  let continuestmt                          = constant "CXCursor_ContinueStmt" int64_t                  
  let breakstmt                             = constant "CXCursor_BreakStmt" int64_t                     
  let returnstmt                            = constant "CXCursor_ReturnStmt" int64_t                    
  let gccasmstmt                            = constant "CXCursor_GCCAsmStmt" int64_t                    
  let asmstmt                               = constant "CXCursor_AsmStmt" int64_t                       
  let objcattrystmt                         = constant "CXCursor_ObjCAtTryStmt" int64_t                 
  let objcatcatchstmt                       = constant "CXCursor_ObjCAtCatchStmt" int64_t               
  let objcatfinallystmt                     = constant "CXCursor_ObjCAtFinallyStmt" int64_t             
  let objcatthrowstmt                       = constant "CXCursor_ObjCAtThrowStmt" int64_t               
  let objcatsynchronizedstmt                = constant "CXCursor_ObjCAtSynchronizedStmt" int64_t        
  let objcautoreleasepoolstmt               = constant "CXCursor_ObjCAutoreleasePoolStmt" int64_t       
  let objcforcollectionstmt                 = constant "CXCursor_ObjCForCollectionStmt" int64_t         
  let cxxcatchstmt                          = constant "CXCursor_CXXCatchStmt" int64_t                  
  let cxxtrystmt                            = constant "CXCursor_CXXTryStmt" int64_t                    
  let cxxforrangestmt                       = constant "CXCursor_CXXForRangeStmt" int64_t               
  let sehtrystmt                            = constant "CXCursor_SEHTryStmt" int64_t                    
  let sehexceptstmt                         = constant "CXCursor_SEHExceptStmt" int64_t                 
  let sehfinallystmt                        = constant "CXCursor_SEHFinallyStmt" int64_t                
  let msasmstmt                             = constant "CXCursor_MSAsmStmt" int64_t                     
  let nullstmt                              = constant "CXCursor_NullStmt" int64_t                      
  let declstmt                              = constant "CXCursor_DeclStmt" int64_t                      
  let ompparalleldirective                  = constant "CXCursor_OMPParallelDirective" int64_t          
  let ompsimddirective                      = constant "CXCursor_OMPSimdDirective" int64_t              
  let ompfordirective                       = constant "CXCursor_OMPForDirective" int64_t               
  let ompsectionsdirective                  = constant "CXCursor_OMPSectionsDirective" int64_t          
  let ompsectiondirective                   = constant "CXCursor_OMPSectionDirective" int64_t           
  let ompsingledirective                    = constant "CXCursor_OMPSingleDirective" int64_t            
  let ompparallelfordirective               = constant "CXCursor_OMPParallelForDirective" int64_t       
  let ompparallelsectionsdirective          = constant "CXCursor_OMPParallelSectionsDirective" int64_t  
  let omptaskdirective                      = constant "CXCursor_OMPTaskDirective" int64_t              
  let ompmasterdirective                    = constant "CXCursor_OMPMasterDirective" int64_t            
  let ompcriticaldirective                  = constant "CXCursor_OMPCriticalDirective" int64_t          
  let omptaskyielddirective                 = constant "CXCursor_OMPTaskyieldDirective" int64_t         
  let ompbarrierdirective                   = constant "CXCursor_OMPBarrierDirective" int64_t           
  let omptaskwaitdirective                  = constant "CXCursor_OMPTaskwaitDirective" int64_t          
  let ompflushdirective                     = constant "CXCursor_OMPFlushDirective" int64_t             
  let sehleavestmt                          = constant "CXCursor_SEHLeaveStmt" int64_t                  
  let ompordereddirective                   = constant "CXCursor_OMPOrderedDirective" int64_t           
  let ompatomicdirective                    = constant "CXCursor_OMPAtomicDirective" int64_t            
  let ompforsimddirective                   = constant "CXCursor_OMPForSimdDirective" int64_t           
  let ompparallelforsimddirective           = constant "CXCursor_OMPParallelForSimdDirective" int64_t   
  let omptargetdirective                    = constant "CXCursor_OMPTargetDirective" int64_t            
  let ompteamsdirective                     = constant "CXCursor_OMPTeamsDirective" int64_t             
  let omptaskgroupdirective                 = constant "CXCursor_OMPTaskgroupDirective" int64_t         
  let ompcancellationpointdirective         = constant "CXCursor_OMPCancellationPointDirective" int64_t 
  let ompcanceldirective                    = constant "CXCursor_OMPCancelDirective" int64_t            
  let omptargetdatadirective                = constant "CXCursor_OMPTargetDataDirective" int64_t        
  let omptaskloopdirective                  = constant "CXCursor_OMPTaskLoopDirective" int64_t          
  let omptaskloopsimddirective              = constant "CXCursor_OMPTaskLoopSimdDirective" int64_t      
  let ompdistributedirective                = constant "CXCursor_OMPDistributeDirective" int64_t        
  let omptargetenterdatadirective           = constant "CXCursor_OMPTargetEnterDataDirective" int64_t   
  let omptargetexitdatadirective            = constant "CXCursor_OMPTargetExitDataDirective" int64_t    
  let omptargetparalleldirective            = constant "CXCursor_OMPTargetParallelDirective" int64_t    
  let omptargetparallelfordirective         = constant "CXCursor_OMPTargetParallelForDirective" int64_t 
  let omptargetupdatedirective              = constant "CXCursor_OMPTargetUpdateDirective" int64_t      
  let ompdistributeparallelfordirective     = constant "CXCursor_OMPDistributeParallelForDirective" int64_t 
  let ompdistributeparallelforsimddirective = constant "CXCursor_OMPDistributeParallelForSimdDirective" int64_t 
  let ompdistributesimddirective            = constant "CXCursor_OMPDistributeSimdDirective" int64_t 
  let omptargetparallelforsimddirective     = constant "CXCursor_OMPTargetParallelForSimdDirective" int64_t 
  let laststmt                              = constant "CXCursor_LastStmt" int64_t 
  let translationunit                       = constant "CXCursor_TranslationUnit" int64_t               
  let firstattr                             = constant "CXCursor_FirstAttr" int64_t                     
  let unexposedattr                         = constant "CXCursor_UnexposedAttr" int64_t                 
  let ibactionattr                          = constant "CXCursor_IBActionAttr" int64_t                  
  let iboutletattr                          = constant "CXCursor_IBOutletAttr" int64_t                  
  let iboutletcollectionattr                = constant "CXCursor_IBOutletCollectionAttr" int64_t        
  let cxxfinalattr                          = constant "CXCursor_CXXFinalAttr" int64_t                  
  let cxxoverrideattr                       = constant "CXCursor_CXXOverrideAttr" int64_t               
  let annotateattr                          = constant "CXCursor_AnnotateAttr" int64_t                  
  let asmlabelattr                          = constant "CXCursor_AsmLabelAttr" int64_t                  
  let packedattr                            = constant "CXCursor_PackedAttr" int64_t                    
  let pureattr                              = constant "CXCursor_PureAttr" int64_t                      
  let constattr                             = constant "CXCursor_ConstAttr" int64_t                     
  let noduplicateattr                       = constant "CXCursor_NoDuplicateAttr" int64_t               
  let cudaconstantattr                      = constant "CXCursor_CUDAConstantAttr" int64_t              
  let cudadeviceattr                        = constant "CXCursor_CUDADeviceAttr" int64_t                
  let cudaglobalattr                        = constant "CXCursor_CUDAGlobalAttr" int64_t                
  let cudahostattr                          = constant "CXCursor_CUDAHostAttr" int64_t                  
  let cudasharedattr                        = constant "CXCursor_CUDASharedAttr" int64_t                
  let visibilityattr                        = constant "CXCursor_VisibilityAttr" int64_t                
  let dllexport                             = constant "CXCursor_DLLExport" int64_t                     
  let dllimport                             = constant "CXCursor_DLLImport" int64_t                     
  let lastattr                              = constant "CXCursor_LastAttr" int64_t                      
  let preprocessingdirective                = constant "CXCursor_PreprocessingDirective" int64_t        
  let macrodefinition                       = constant "CXCursor_MacroDefinition" int64_t               
  let macroexpansion                        = constant "CXCursor_MacroExpansion" int64_t                
  let macroinstantiation                    = constant "CXCursor_MacroInstantiation" int64_t            
  let inclusiondirective                    = constant "CXCursor_InclusionDirective" int64_t            
  let firstpreprocessing                    = constant "CXCursor_FirstPreprocessing" int64_t            
  let lastpreprocessing                     = constant "CXCursor_LastPreprocessing" int64_t             
  let moduleimportdecl                      = constant "CXCursor_ModuleImportDecl" int64_t              
  let typealiastemplatedecl                 = constant "CXCursor_TypeAliasTemplateDecl" int64_t         
  let staticassert                          = constant "CXCursor_StaticAssert" int64_t                  
  let firstextradecl                        = constant "CXCursor_FirstExtraDecl" int64_t                
  let lastextradecl                         = constant "CXCursor_LastExtraDecl" int64_t                 
  let overloadcandidate                     = constant "CXCursor_OverloadCandidate" int64_t             



  let cursor_kind = enum "CXCursorKind" [
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

end
