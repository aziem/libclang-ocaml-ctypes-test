open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated_types)(Ffi_generated)

open Ffi_bindings
open B

let bool_val b =
  if b then 1 else 0

type cx_cursor_kind = cursor_kind

module Util =
  struct 
    let version () =
      B.getcstring_ (B.version_ ())
      
    let toggle_crash_recovery flag =
      B.toggle_crash_recovery_ flag
  end


module Cursor =
struct

  type visit_result =
    | Break
    | Continue
    | Recurse

  let visit_result = function
    | Break -> Ffi_bindings.CXChildVisitBreak
    | Continue -> Ffi_bindings.CXChildVisitContinue
    | Recurse -> Ffi_bindings.CXChildVisitRecurse


  type kind =
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

  let cx_cursor_kind_of_kind = function
    | UnexposedDecl                         -> Ffi_bindings.CXCursor_UnexposedDecl                 
    | StructDecl                            -> Ffi_bindings.CXCursor_StructDecl                    
    | UnionDecl                             -> Ffi_bindings.CXCursor_UnionDecl                     
    | ClassDecl                             -> Ffi_bindings.CXCursor_ClassDecl                     
    | EnumDecl                              -> Ffi_bindings.CXCursor_EnumDecl                      
    | FieldDecl                             -> Ffi_bindings.CXCursor_FieldDecl                     
    | EnumConstantDecl                      -> Ffi_bindings.CXCursor_EnumConstantDecl              
    | FunctionDecl                          -> Ffi_bindings.CXCursor_FunctionDecl                  
    | VarDecl                               -> Ffi_bindings.CXCursor_VarDecl                       
    | ParmDecl                              -> Ffi_bindings.CXCursor_ParmDecl                      
    | ObjCInterfaceDecl                     -> Ffi_bindings.CXCursor_ObjCInterfaceDecl             
    | ObjCCategoryDecl -> Ffi_bindings.CXCursor_ObjCCategoryDecl             
    | ObjCProtocolDecl                      -> Ffi_bindings.CXCursor_ObjCProtocolDecl              
    | ObjCPropertyDecl                      -> Ffi_bindings.CXCursor_ObjCPropertyDecl              
    | ObjCIvarDecl                          -> Ffi_bindings.CXCursor_ObjCIvarDecl                  
    | ObjCInstanceMethodDecl                -> Ffi_bindings.CXCursor_ObjCInstanceMethodDecl        
    | ObjCClassMethodDecl                   -> Ffi_bindings.CXCursor_ObjCClassMethodDecl           
    | ObjCImplementationDecl                -> Ffi_bindings.CXCursor_ObjCImplementationDecl        
    | ObjCCategoryImplDecl                  -> Ffi_bindings.CXCursor_ObjCCategoryImplDecl          
    | TypedefDecl                           -> Ffi_bindings.CXCursor_TypedefDecl                   
    | CXXMethod                             -> Ffi_bindings.CXCursor_CXXMethod                     
    | Namespace                             -> Ffi_bindings.CXCursor_Namespace                     
    | LinkageSpec                           -> Ffi_bindings.CXCursor_LinkageSpec                   
    | Constructor                           -> Ffi_bindings.CXCursor_Constructor                   
    | Destructor                            -> Ffi_bindings.CXCursor_Destructor                    
    | ConversionFunction                    -> Ffi_bindings.CXCursor_ConversionFunction            
    | TemplateTypeParameter                 -> Ffi_bindings.CXCursor_TemplateTypeParameter         
    | NonTypeTemplateParameter              -> Ffi_bindings.CXCursor_NonTypeTemplateParameter      
    | TemplateTemplateParameter             -> Ffi_bindings.CXCursor_TemplateTemplateParameter     
    | FunctionTemplate                      -> Ffi_bindings.CXCursor_FunctionTemplate              
    | ClassTemplate                         -> Ffi_bindings.CXCursor_ClassTemplate                 
    | ClassTemplatePartialSpecialization    -> Ffi_bindings.CXCursor_ClassTemplatePartialSpecialization 
    | NamespaceAlias                        -> Ffi_bindings.CXCursor_NamespaceAlias                
    | UsingDirective                        -> Ffi_bindings.CXCursor_UsingDirective                
    | UsingDeclaration                      -> Ffi_bindings.CXCursor_UsingDeclaration              
    | TypeAliasDecl                         -> Ffi_bindings.CXCursor_TypeAliasDecl                 
    | ObjCSynthesizeDecl                    -> Ffi_bindings.CXCursor_ObjCSynthesizeDecl            
    | ObjCDynamicDecl                       -> Ffi_bindings.CXCursor_ObjCDynamicDecl               
    | CXXAccessSpecifier                    -> Ffi_bindings.CXCursor_CXXAccessSpecifier            
    | FirstDecl                             -> Ffi_bindings.CXCursor_FirstDecl                     
    | LastDecl                              -> Ffi_bindings.CXCursor_LastDecl                      
    | FirstRef                              -> Ffi_bindings.CXCursor_FirstRef                      
    | ObjCSuperClassRef                     -> Ffi_bindings.CXCursor_ObjCSuperClassRef             
    | ObjCProtocolRef                       -> Ffi_bindings.CXCursor_ObjCProtocolRef               
    | ObjCClassRef                          -> Ffi_bindings.CXCursor_ObjCClassRef                  
    | TypeRef                               -> Ffi_bindings.CXCursor_TypeRef                       
    | CXXBaseSpecifier                      -> Ffi_bindings.CXCursor_CXXBaseSpecifier              
    | TemplateRef                           -> Ffi_bindings.CXCursor_TemplateRef                   
    | NamespaceRef                          -> Ffi_bindings.CXCursor_NamespaceRef                  
    | MemberRef                             -> Ffi_bindings.CXCursor_MemberRef                     
    | LabelRef                              -> Ffi_bindings.CXCursor_LabelRef                      
    | OverloadedDeclRef                     -> Ffi_bindings.CXCursor_OverloadedDeclRef             
    | VariableRef                           -> Ffi_bindings.CXCursor_VariableRef                   
    | LastRef                               -> Ffi_bindings.CXCursor_LastRef                       
    | FirstInvalid                          -> Ffi_bindings.CXCursor_FirstInvalid                  
    | InvalidFile                           -> Ffi_bindings.CXCursor_InvalidFile                   
    | NoDeclFound                           -> Ffi_bindings.CXCursor_NoDeclFound                   
    | NotImplemented                        -> Ffi_bindings.CXCursor_NotImplemented                
    | InvalidCode                           -> Ffi_bindings.CXCursor_InvalidCode                   
    | LastInvalid                           -> Ffi_bindings.CXCursor_LastInvalid                   
    | FirstExpr                             -> Ffi_bindings.CXCursor_FirstExpr                     
    | UnexposedExpr                         -> Ffi_bindings.CXCursor_UnexposedExpr                 
    | DeclRefExpr                           -> Ffi_bindings.CXCursor_DeclRefExpr                   
    | MemberRefExpr                         -> Ffi_bindings.CXCursor_MemberRefExpr                 
    | CallExpr                              -> Ffi_bindings.CXCursor_CallExpr                      
    | ObjCMessageExpr                       -> Ffi_bindings.CXCursor_ObjCMessageExpr               
    | BlockExpr                             -> Ffi_bindings.CXCursor_BlockExpr                     
    | IntegerLiteral                        -> Ffi_bindings.CXCursor_IntegerLiteral                
    | FloatingLiteral                       -> Ffi_bindings.CXCursor_FloatingLiteral               
    | ImaginaryLiteral                      -> Ffi_bindings.CXCursor_ImaginaryLiteral              
    | StringLiteral                         -> Ffi_bindings.CXCursor_StringLiteral                 
    | CharacterLiteral                      -> Ffi_bindings.CXCursor_CharacterLiteral              
    | ParenExpr                             -> Ffi_bindings.CXCursor_ParenExpr                     
    | UnaryOperator                         -> Ffi_bindings.CXCursor_UnaryOperator                 
    | ArraySubscriptExpr                    -> Ffi_bindings.CXCursor_ArraySubscriptExpr            
    | BinaryOperator                        -> Ffi_bindings.CXCursor_BinaryOperator                
    | CompoundAssignOperator                -> Ffi_bindings.CXCursor_CompoundAssignOperator        
    | ConditionalOperator                   -> Ffi_bindings.CXCursor_ConditionalOperator           
    | CStyleCastExpr                        -> Ffi_bindings.CXCursor_CStyleCastExpr                
    | CompoundLiteralExpr                   -> Ffi_bindings.CXCursor_CompoundLiteralExpr           
    | InitListExpr                          -> Ffi_bindings.CXCursor_InitListExpr                  
    | AddrLabelExpr                         -> Ffi_bindings.CXCursor_AddrLabelExpr                 
    | StmtExpr                              -> Ffi_bindings.CXCursor_StmtExpr                      
    | GenericSelectionExpr                  -> Ffi_bindings.CXCursor_GenericSelectionExpr          
    | GNUNullExpr                           -> Ffi_bindings.CXCursor_GNUNullExpr                   
    | CXXStaticCastExpr                     -> Ffi_bindings.CXCursor_CXXStaticCastExpr             
    | CXXDynamicCastExpr                    -> Ffi_bindings.CXCursor_CXXDynamicCastExpr            
    | CXXReinterpretCastExpr                -> Ffi_bindings.CXCursor_CXXReinterpretCastExpr        
    | CXXConstCastExpr                      -> Ffi_bindings.CXCursor_CXXConstCastExpr              
    | CXXFunctionalCastExpr                 -> Ffi_bindings.CXCursor_CXXFunctionalCastExpr         
    | CXXTypeidExpr                         -> Ffi_bindings.CXCursor_CXXTypeidExpr                 
    | CXXBoolLiteralExpr                    -> Ffi_bindings.CXCursor_CXXBoolLiteralExpr            
    | CXXNullPtrLiteralExpr                 -> Ffi_bindings.CXCursor_CXXNullPtrLiteralExpr         
    | CXXThisExpr                           -> Ffi_bindings.CXCursor_CXXThisExpr                   
    | CXXThrowExpr                          -> Ffi_bindings.CXCursor_CXXThrowExpr                  
    | CXXNewExpr                            -> Ffi_bindings.CXCursor_CXXNewExpr                    
    | CXXDeleteExpr                         -> Ffi_bindings.CXCursor_CXXDeleteExpr                 
    | UnaryExpr                             -> Ffi_bindings.CXCursor_UnaryExpr                     
    | ObjCStringLiteral                     -> Ffi_bindings.CXCursor_ObjCStringLiteral             
    | ObjCEncodeExpr                        -> Ffi_bindings.CXCursor_ObjCEncodeExpr                
    | ObjCSelectorExpr                      -> Ffi_bindings.CXCursor_ObjCSelectorExpr              
    | ObjCProtocolExpr                      -> Ffi_bindings.CXCursor_ObjCProtocolExpr              
    | ObjCBridgedCastExpr                   -> Ffi_bindings.CXCursor_ObjCBridgedCastExpr           
    | PackExpansionExpr                     -> Ffi_bindings.CXCursor_PackExpansionExpr             
    | SizeOfPackExpr                        -> Ffi_bindings.CXCursor_SizeOfPackExpr                
    | LambdaExpr                            -> Ffi_bindings.CXCursor_LambdaExpr                    
    | ObjCBoolLiteralExpr                   -> Ffi_bindings.CXCursor_ObjCBoolLiteralExpr           
    | ObjCSelfExpr                          -> Ffi_bindings.CXCursor_ObjCSelfExpr                  
    | OMPArraySectionExpr                   -> Ffi_bindings.CXCursor_OMPArraySectionExpr           
    | ObjCAvailabilityCheckExpr             -> Ffi_bindings.CXCursor_ObjCAvailabilityCheckExpr     
    | LastExpr                              -> Ffi_bindings.CXCursor_LastExpr                      
    | FirstStmt                             -> Ffi_bindings.CXCursor_FirstStmt                     
    | UnexposedStmt                         -> Ffi_bindings.CXCursor_UnexposedStmt                 
    | LabelStmt                             -> Ffi_bindings.CXCursor_LabelStmt                     
    | CompoundStmt                          -> Ffi_bindings.CXCursor_CompoundStmt                  
    | CaseStmt                              -> Ffi_bindings.CXCursor_CaseStmt                      
    | DefaultStmt                           -> Ffi_bindings.CXCursor_DefaultStmt                   
    | IfStmt                                -> Ffi_bindings.CXCursor_IfStmt                        
    | SwitchStmt                            -> Ffi_bindings.CXCursor_SwitchStmt                    
    | WhileStmt                             -> Ffi_bindings.CXCursor_WhileStmt                     
    | DoStmt                                -> Ffi_bindings.CXCursor_DoStmt                        
    | ForStmt                               -> Ffi_bindings.CXCursor_ForStmt                       
    | GotoStmt                              -> Ffi_bindings.CXCursor_GotoStmt                      
    | IndirectGotoStmt                      -> Ffi_bindings.CXCursor_IndirectGotoStmt              
    | ContinueStmt                          -> Ffi_bindings.CXCursor_ContinueStmt                  
    | BreakStmt                             -> Ffi_bindings.CXCursor_BreakStmt                     
    | ReturnStmt                            -> Ffi_bindings.CXCursor_ReturnStmt                    
    | GCCAsmStmt                            -> Ffi_bindings.CXCursor_GCCAsmStmt                    
    | AsmStmt                               -> Ffi_bindings.CXCursor_AsmStmt                       
    | ObjCAtTryStmt                         -> Ffi_bindings.CXCursor_ObjCAtTryStmt                 
    | ObjCAtCatchStmt                       -> Ffi_bindings.CXCursor_ObjCAtCatchStmt               
    | ObjCAtFinallyStmt                     -> Ffi_bindings.CXCursor_ObjCAtFinallyStmt             
    | ObjCAtThrowStmt                       -> Ffi_bindings.CXCursor_ObjCAtThrowStmt               
    | ObjCAtSynchronizedStmt                -> Ffi_bindings.CXCursor_ObjCAtSynchronizedStmt        
    | ObjCAutoreleasePoolStmt               -> Ffi_bindings.CXCursor_ObjCAutoreleasePoolStmt       
    | ObjCForCollectionStmt                 -> Ffi_bindings.CXCursor_ObjCForCollectionStmt         
    | CXXCatchStmt                          -> Ffi_bindings.CXCursor_CXXCatchStmt                  
    | CXXTryStmt                            -> Ffi_bindings.CXCursor_CXXTryStmt                    
    | CXXForRangeStmt                       -> Ffi_bindings.CXCursor_CXXForRangeStmt               
    | SEHTryStmt                            -> Ffi_bindings.CXCursor_SEHTryStmt                    
    | SEHExceptStmt                         -> Ffi_bindings.CXCursor_SEHExceptStmt                 
    | SEHFinallyStmt                        -> Ffi_bindings.CXCursor_SEHFinallyStmt                
    | MSAsmStmt                             -> Ffi_bindings.CXCursor_MSAsmStmt                     
    | NullStmt                              -> Ffi_bindings.CXCursor_NullStmt                      
    | DeclStmt                              -> Ffi_bindings.CXCursor_DeclStmt                      
    | OMPParallelDirective                  -> Ffi_bindings.CXCursor_OMPParallelDirective          
    | OMPSimdDirective                      -> Ffi_bindings.CXCursor_OMPSimdDirective              
    | OMPForDirective                       -> Ffi_bindings.CXCursor_OMPForDirective               
    | OMPSectionsDirective                  -> Ffi_bindings.CXCursor_OMPSectionsDirective          
    | OMPSectionDirective                   -> Ffi_bindings.CXCursor_OMPSectionDirective           
    | OMPSingleDirective                    -> Ffi_bindings.CXCursor_OMPSingleDirective            
    | OMPParallelForDirective               -> Ffi_bindings.CXCursor_OMPParallelForDirective       
    | OMPParallelSectionsDirective          -> Ffi_bindings.CXCursor_OMPParallelSectionsDirective  
    | OMPTaskDirective                      -> Ffi_bindings.CXCursor_OMPTaskDirective              
    | OMPMasterDirective                    -> Ffi_bindings.CXCursor_OMPMasterDirective            
    | OMPCriticalDirective                  -> Ffi_bindings.CXCursor_OMPCriticalDirective          
    | OMPTaskyieldDirective                 -> Ffi_bindings.CXCursor_OMPTaskyieldDirective         
    | OMPBarrierDirective                   -> Ffi_bindings.CXCursor_OMPBarrierDirective           
    | OMPTaskwaitDirective                  -> Ffi_bindings.CXCursor_OMPTaskwaitDirective          
    | OMPFlushDirective                     -> Ffi_bindings.CXCursor_OMPFlushDirective             
    | SEHLeaveStmt                          -> Ffi_bindings.CXCursor_SEHLeaveStmt                  
    | OMPOrderedDirective                   -> Ffi_bindings.CXCursor_OMPOrderedDirective           
    | OMPAtomicDirective                    -> Ffi_bindings.CXCursor_OMPAtomicDirective            
    | OMPForSimdDirective                   -> Ffi_bindings.CXCursor_OMPForSimdDirective           
    | OMPParallelForSimdDirective           -> Ffi_bindings.CXCursor_OMPParallelForSimdDirective   
    | OMPTargetDirective                    -> Ffi_bindings.CXCursor_OMPTargetDirective            
    | OMPTeamsDirective                     -> Ffi_bindings.CXCursor_OMPTeamsDirective             
    | OMPTaskgroupDirective                 -> Ffi_bindings.CXCursor_OMPTaskgroupDirective         
    | OMPCancellationPointDirective         -> Ffi_bindings.CXCursor_OMPCancellationPointDirective 
    | OMPCancelDirective                    -> Ffi_bindings.CXCursor_OMPCancelDirective            
    | OMPTargetDataDirective                -> Ffi_bindings.CXCursor_OMPTargetDataDirective        
    | OMPTaskLoopDirective                  -> Ffi_bindings.CXCursor_OMPTaskLoopDirective          
    | OMPTaskLoopSimdDirective              -> Ffi_bindings.CXCursor_OMPTaskLoopSimdDirective      
    | OMPDistributeDirective                -> Ffi_bindings.CXCursor_OMPDistributeDirective        
    | OMPTargetEnterDataDirective           -> Ffi_bindings.CXCursor_OMPTargetEnterDataDirective   
    | OMPTargetExitDataDirective            -> Ffi_bindings.CXCursor_OMPTargetExitDataDirective    
    | OMPTargetParallelDirective            -> Ffi_bindings.CXCursor_OMPTargetParallelDirective    
    | OMPTargetParallelForDirective         -> Ffi_bindings.CXCursor_OMPTargetParallelForDirective 
    | OMPTargetUpdateDirective              -> Ffi_bindings.CXCursor_OMPTargetUpdateDirective      
    | OMPDistributeParallelForDirective     -> Ffi_bindings.CXCursor_OMPDistributeParallelForDirective 
    | OMPDistributeParallelForSimdDirective -> Ffi_bindings.CXCursor_OMPDistributeParallelForSimdDirective 
    | OMPDistributeSimdDirective            -> Ffi_bindings.CXCursor_OMPDistributeSimdDirective 
    | OMPTargetParallelForSimdDirective     -> Ffi_bindings.CXCursor_OMPTargetParallelForSimdDirective 
    | LastStmt                              -> Ffi_bindings.CXCursor_LastStmt 
    | TranslationUnit                       -> Ffi_bindings.CXCursor_TranslationUnit               
    | FirstAttr                             -> Ffi_bindings.CXCursor_FirstAttr                     
    | UnexposedAttr                         -> Ffi_bindings.CXCursor_UnexposedAttr                 
    | IBActionAttr                          -> Ffi_bindings.CXCursor_IBActionAttr                  
    | IBOutletAttr                          -> Ffi_bindings.CXCursor_IBOutletAttr                  
    | IBOutletCollectionAttr                -> Ffi_bindings.CXCursor_IBOutletCollectionAttr        
    | CXXFinalAttr                          -> Ffi_bindings.CXCursor_CXXFinalAttr                  
    | CXXOverrideAttr                       -> Ffi_bindings.CXCursor_CXXOverrideAttr               
    | AnnotateAttr                          -> Ffi_bindings.CXCursor_AnnotateAttr                  
    | AsmLabelAttr                          -> Ffi_bindings.CXCursor_AsmLabelAttr                  
    | PackedAttr                            -> Ffi_bindings.CXCursor_PackedAttr                    
    | PureAttr                              -> Ffi_bindings.CXCursor_PureAttr                      
    | ConstAttr                             -> Ffi_bindings.CXCursor_ConstAttr                     
    | NoDuplicateAttr                       -> Ffi_bindings.CXCursor_NoDuplicateAttr               
    | CUDAConstantAttr                      -> Ffi_bindings.CXCursor_CUDAConstantAttr              
    | CUDADeviceAttr                        -> Ffi_bindings.CXCursor_CUDADeviceAttr                
    | CUDAGlobalAttr                        -> Ffi_bindings.CXCursor_CUDAGlobalAttr                
    | CUDAHostAttr                          -> Ffi_bindings.CXCursor_CUDAHostAttr                  
    | CUDASharedAttr                        -> Ffi_bindings.CXCursor_CUDASharedAttr                
    | VisibilityAttr                        -> Ffi_bindings.CXCursor_VisibilityAttr                
    | DLLExport                             -> Ffi_bindings.CXCursor_DLLExport                     
    | DLLImport                             -> Ffi_bindings.CXCursor_DLLImport                     
    | LastAttr                              -> Ffi_bindings.CXCursor_LastAttr                      
    | PreprocessingDirective                -> Ffi_bindings.CXCursor_PreprocessingDirective        
    | MacroDefinition                       -> Ffi_bindings.CXCursor_MacroDefinition               
    | MacroExpansion                        -> Ffi_bindings.CXCursor_MacroExpansion                
    | MacroInstantiation                    -> Ffi_bindings.CXCursor_MacroInstantiation            
    | InclusionDirective                    -> Ffi_bindings.CXCursor_InclusionDirective            
    | FirstPreprocessing                    -> Ffi_bindings.CXCursor_FirstPreprocessing            
    | LastPreprocessing                     -> Ffi_bindings.CXCursor_LastPreprocessing             
    | ModuleImportDecl                      -> Ffi_bindings.CXCursor_ModuleImportDecl              
    | TypeAliasTemplateDecl                 -> Ffi_bindings.CXCursor_TypeAliasTemplateDecl         
    | StaticAssert                          -> Ffi_bindings.CXCursor_StaticAssert                  
    | FirstExtraDecl                        -> Ffi_bindings.CXCursor_FirstExtraDecl                
    | LastExtraDecl                         -> Ffi_bindings.CXCursor_LastExtraDecl                 
    | OverloadCandidate                     -> Ffi_bindings.CXCursor_OverloadCandidate  



  let kind_of_cx_cursor_kind = function
    | Ffi_bindings.CXCursor_UnexposedDecl -> UnexposedDecl
    | Ffi_bindings.CXCursor_StructDecl -> StructDecl
    | Ffi_bindings.CXCursor_UnionDecl -> UnionDecl
    | Ffi_bindings.CXCursor_ClassDecl -> ClassDecl
    | Ffi_bindings.CXCursor_EnumDecl -> EnumDecl
    | Ffi_bindings.CXCursor_FieldDecl -> FieldDecl
    | Ffi_bindings.CXCursor_EnumConstantDecl -> EnumConstantDecl
    | Ffi_bindings.CXCursor_FunctionDecl -> FunctionDecl
    | Ffi_bindings.CXCursor_VarDecl -> VarDecl
    | Ffi_bindings.CXCursor_ParmDecl -> ParmDecl
    | Ffi_bindings.CXCursor_ObjCInterfaceDecl -> ObjCInterfaceDecl
    | Ffi_bindings.CXCursor_ObjCCategoryDecl -> ObjCCategoryDecl
    | Ffi_bindings.CXCursor_ObjCProtocolDecl -> ObjCProtocolDecl
    | Ffi_bindings.CXCursor_ObjCPropertyDecl -> ObjCPropertyDecl
    | Ffi_bindings.CXCursor_ObjCIvarDecl -> ObjCIvarDecl
    | Ffi_bindings.CXCursor_ObjCInstanceMethodDecl -> ObjCInstanceMethodDecl
    | Ffi_bindings.CXCursor_ObjCClassMethodDecl -> ObjCClassMethodDecl
    | Ffi_bindings.CXCursor_ObjCImplementationDecl -> ObjCImplementationDecl
    | Ffi_bindings.CXCursor_ObjCCategoryImplDecl -> ObjCCategoryImplDecl
    | Ffi_bindings.CXCursor_TypedefDecl -> TypedefDecl
    | Ffi_bindings.CXCursor_CXXMethod -> CXXMethod
    | Ffi_bindings.CXCursor_Namespace -> Namespace
    | Ffi_bindings.CXCursor_LinkageSpec -> LinkageSpec
    | Ffi_bindings.CXCursor_Constructor -> Constructor
    | Ffi_bindings.CXCursor_Destructor -> Destructor
    | Ffi_bindings.CXCursor_ConversionFunction -> ConversionFunction
    | Ffi_bindings.CXCursor_TemplateTypeParameter -> TemplateTypeParameter
    | Ffi_bindings.CXCursor_NonTypeTemplateParameter -> NonTypeTemplateParameter
    | Ffi_bindings.CXCursor_TemplateTemplateParameter -> TemplateTemplateParameter
    | Ffi_bindings.CXCursor_FunctionTemplate -> FunctionTemplate
    | Ffi_bindings.CXCursor_ClassTemplate -> ClassTemplate
    | Ffi_bindings.CXCursor_ClassTemplatePartialSpecialization -> ClassTemplatePartialSpecialization
    | Ffi_bindings.CXCursor_NamespaceAlias -> NamespaceAlias
    | Ffi_bindings.CXCursor_UsingDirective -> UsingDirective
    | Ffi_bindings.CXCursor_UsingDeclaration -> UsingDeclaration
    | Ffi_bindings.CXCursor_TypeAliasDecl -> TypeAliasDecl
    | Ffi_bindings.CXCursor_ObjCSynthesizeDecl -> ObjCSynthesizeDecl
    | Ffi_bindings.CXCursor_ObjCDynamicDecl -> ObjCDynamicDecl
    | Ffi_bindings.CXCursor_CXXAccessSpecifier -> CXXAccessSpecifier
    | Ffi_bindings.CXCursor_FirstDecl -> FirstDecl
    | Ffi_bindings.CXCursor_LastDecl -> LastDecl
    | Ffi_bindings.CXCursor_FirstRef -> FirstRef
    | Ffi_bindings.CXCursor_ObjCSuperClassRef -> ObjCSuperClassRef
    | Ffi_bindings.CXCursor_ObjCProtocolRef -> ObjCProtocolRef
    | Ffi_bindings.CXCursor_ObjCClassRef -> ObjCClassRef
    | Ffi_bindings.CXCursor_TypeRef -> TypeRef
    | Ffi_bindings.CXCursor_CXXBaseSpecifier -> CXXBaseSpecifier
    | Ffi_bindings.CXCursor_TemplateRef -> TemplateRef
    | Ffi_bindings.CXCursor_NamespaceRef -> NamespaceRef
    | Ffi_bindings.CXCursor_MemberRef -> MemberRef
    | Ffi_bindings.CXCursor_LabelRef -> LabelRef
    | Ffi_bindings.CXCursor_OverloadedDeclRef -> OverloadedDeclRef
    | Ffi_bindings.CXCursor_VariableRef -> VariableRef
    | Ffi_bindings.CXCursor_LastRef -> LastRef
    | Ffi_bindings.CXCursor_FirstInvalid -> FirstInvalid
    | Ffi_bindings.CXCursor_InvalidFile -> InvalidFile
    | Ffi_bindings.CXCursor_NoDeclFound -> NoDeclFound
    | Ffi_bindings.CXCursor_NotImplemented -> NotImplemented
    | Ffi_bindings.CXCursor_InvalidCode -> InvalidCode
    | Ffi_bindings.CXCursor_LastInvalid -> LastInvalid
    | Ffi_bindings.CXCursor_FirstExpr -> FirstExpr
    | Ffi_bindings.CXCursor_UnexposedExpr -> UnexposedExpr
    | Ffi_bindings.CXCursor_DeclRefExpr -> DeclRefExpr
    | Ffi_bindings.CXCursor_MemberRefExpr -> MemberRefExpr
    | Ffi_bindings.CXCursor_CallExpr -> CallExpr
    | Ffi_bindings.CXCursor_ObjCMessageExpr -> ObjCMessageExpr
    | Ffi_bindings.CXCursor_BlockExpr -> BlockExpr
    | Ffi_bindings.CXCursor_IntegerLiteral -> IntegerLiteral
    | Ffi_bindings.CXCursor_FloatingLiteral -> FloatingLiteral
    | Ffi_bindings.CXCursor_ImaginaryLiteral -> ImaginaryLiteral
    | Ffi_bindings.CXCursor_StringLiteral -> StringLiteral
    | Ffi_bindings.CXCursor_CharacterLiteral -> CharacterLiteral
    | Ffi_bindings.CXCursor_ParenExpr -> ParenExpr
    | Ffi_bindings.CXCursor_UnaryOperator -> UnaryOperator
    | Ffi_bindings.CXCursor_ArraySubscriptExpr -> ArraySubscriptExpr
    | Ffi_bindings.CXCursor_BinaryOperator -> BinaryOperator
    | Ffi_bindings.CXCursor_CompoundAssignOperator -> CompoundAssignOperator
    | Ffi_bindings.CXCursor_ConditionalOperator -> ConditionalOperator
    | Ffi_bindings.CXCursor_CStyleCastExpr -> CStyleCastExpr
    | Ffi_bindings.CXCursor_CompoundLiteralExpr -> CompoundLiteralExpr
    | Ffi_bindings.CXCursor_InitListExpr -> InitListExpr
    | Ffi_bindings.CXCursor_AddrLabelExpr -> AddrLabelExpr
    | Ffi_bindings.CXCursor_StmtExpr -> StmtExpr
    | Ffi_bindings.CXCursor_GenericSelectionExpr -> GenericSelectionExpr
    | Ffi_bindings.CXCursor_GNUNullExpr -> GNUNullExpr
    | Ffi_bindings.CXCursor_CXXStaticCastExpr -> CXXStaticCastExpr
    | Ffi_bindings.CXCursor_CXXDynamicCastExpr -> CXXDynamicCastExpr
    | Ffi_bindings.CXCursor_CXXReinterpretCastExpr -> CXXReinterpretCastExpr
    | Ffi_bindings.CXCursor_CXXConstCastExpr -> CXXConstCastExpr
    | Ffi_bindings.CXCursor_CXXFunctionalCastExpr -> CXXFunctionalCastExpr
    | Ffi_bindings.CXCursor_CXXTypeidExpr -> CXXTypeidExpr
    | Ffi_bindings.CXCursor_CXXBoolLiteralExpr -> CXXBoolLiteralExpr
    | Ffi_bindings.CXCursor_CXXNullPtrLiteralExpr -> CXXNullPtrLiteralExpr
    | Ffi_bindings.CXCursor_CXXThisExpr -> CXXThisExpr
    | Ffi_bindings.CXCursor_CXXThrowExpr -> CXXThrowExpr
    | Ffi_bindings.CXCursor_CXXNewExpr -> CXXNewExpr
    | Ffi_bindings.CXCursor_CXXDeleteExpr -> CXXDeleteExpr
    | Ffi_bindings.CXCursor_UnaryExpr -> UnaryExpr
    | Ffi_bindings.CXCursor_ObjCStringLiteral -> ObjCStringLiteral
    | Ffi_bindings.CXCursor_ObjCEncodeExpr -> ObjCEncodeExpr
    | Ffi_bindings.CXCursor_ObjCSelectorExpr -> ObjCSelectorExpr
    | Ffi_bindings.CXCursor_ObjCProtocolExpr -> ObjCProtocolExpr
    | Ffi_bindings.CXCursor_ObjCBridgedCastExpr -> ObjCBridgedCastExpr
    | Ffi_bindings.CXCursor_PackExpansionExpr -> PackExpansionExpr
    | Ffi_bindings.CXCursor_SizeOfPackExpr -> SizeOfPackExpr
    | Ffi_bindings.CXCursor_LambdaExpr -> LambdaExpr
    | Ffi_bindings.CXCursor_ObjCBoolLiteralExpr -> ObjCBoolLiteralExpr
    | Ffi_bindings.CXCursor_ObjCSelfExpr -> ObjCSelfExpr
    | Ffi_bindings.CXCursor_OMPArraySectionExpr -> OMPArraySectionExpr
    | Ffi_bindings.CXCursor_ObjCAvailabilityCheckExpr -> ObjCAvailabilityCheckExpr
    | Ffi_bindings.CXCursor_LastExpr -> LastExpr
    | Ffi_bindings.CXCursor_FirstStmt -> FirstStmt
    | Ffi_bindings.CXCursor_UnexposedStmt -> UnexposedStmt
    | Ffi_bindings.CXCursor_LabelStmt -> LabelStmt
    | Ffi_bindings.CXCursor_CompoundStmt -> CompoundStmt
    | Ffi_bindings.CXCursor_CaseStmt -> CaseStmt
    | Ffi_bindings.CXCursor_DefaultStmt -> DefaultStmt
    | Ffi_bindings.CXCursor_IfStmt -> IfStmt
    | Ffi_bindings.CXCursor_SwitchStmt -> SwitchStmt
    | Ffi_bindings.CXCursor_WhileStmt -> WhileStmt
    | Ffi_bindings.CXCursor_DoStmt -> DoStmt
    | Ffi_bindings.CXCursor_ForStmt -> ForStmt
    | Ffi_bindings.CXCursor_GotoStmt -> GotoStmt
    | Ffi_bindings.CXCursor_IndirectGotoStmt -> IndirectGotoStmt
    | Ffi_bindings.CXCursor_ContinueStmt -> ContinueStmt
    | Ffi_bindings.CXCursor_BreakStmt -> BreakStmt
    | Ffi_bindings.CXCursor_ReturnStmt -> ReturnStmt
    | Ffi_bindings.CXCursor_GCCAsmStmt -> GCCAsmStmt
    | Ffi_bindings.CXCursor_AsmStmt -> AsmStmt
    | Ffi_bindings.CXCursor_ObjCAtTryStmt -> ObjCAtTryStmt
    | Ffi_bindings.CXCursor_ObjCAtCatchStmt -> ObjCAtCatchStmt
    | Ffi_bindings.CXCursor_ObjCAtFinallyStmt -> ObjCAtFinallyStmt
    | Ffi_bindings.CXCursor_ObjCAtThrowStmt -> ObjCAtThrowStmt
    | Ffi_bindings.CXCursor_ObjCAtSynchronizedStmt -> ObjCAtSynchronizedStmt
    | Ffi_bindings.CXCursor_ObjCAutoreleasePoolStmt -> ObjCAutoreleasePoolStmt
    | Ffi_bindings.CXCursor_ObjCForCollectionStmt -> ObjCForCollectionStmt
    | Ffi_bindings.CXCursor_CXXCatchStmt -> CXXCatchStmt
    | Ffi_bindings.CXCursor_CXXTryStmt -> CXXTryStmt
    | Ffi_bindings.CXCursor_CXXForRangeStmt -> CXXForRangeStmt
    | Ffi_bindings.CXCursor_SEHTryStmt -> SEHTryStmt
    | Ffi_bindings.CXCursor_SEHExceptStmt -> SEHExceptStmt
    | Ffi_bindings.CXCursor_SEHFinallyStmt -> SEHFinallyStmt
    | Ffi_bindings.CXCursor_MSAsmStmt -> MSAsmStmt
    | Ffi_bindings.CXCursor_NullStmt -> NullStmt
    | Ffi_bindings.CXCursor_DeclStmt -> DeclStmt
    | Ffi_bindings.CXCursor_OMPParallelDirective -> OMPParallelDirective
    | Ffi_bindings.CXCursor_OMPSimdDirective -> OMPSimdDirective
    | Ffi_bindings.CXCursor_OMPForDirective -> OMPForDirective
    | Ffi_bindings.CXCursor_OMPSectionsDirective -> OMPSectionsDirective
    | Ffi_bindings.CXCursor_OMPSectionDirective -> OMPSectionDirective
    | Ffi_bindings.CXCursor_OMPSingleDirective -> OMPSingleDirective
    | Ffi_bindings.CXCursor_OMPParallelForDirective -> OMPParallelForDirective
    | Ffi_bindings.CXCursor_OMPParallelSectionsDirective -> OMPParallelSectionsDirective
    | Ffi_bindings.CXCursor_OMPTaskDirective -> OMPTaskDirective
    | Ffi_bindings.CXCursor_OMPMasterDirective -> OMPMasterDirective
    | Ffi_bindings.CXCursor_OMPCriticalDirective -> OMPCriticalDirective
    | Ffi_bindings.CXCursor_OMPTaskyieldDirective -> OMPTaskyieldDirective
    | Ffi_bindings.CXCursor_OMPBarrierDirective -> OMPBarrierDirective
    | Ffi_bindings.CXCursor_OMPTaskwaitDirective -> OMPTaskwaitDirective
    | Ffi_bindings.CXCursor_OMPFlushDirective -> OMPFlushDirective
    | Ffi_bindings.CXCursor_SEHLeaveStmt -> SEHLeaveStmt
    | Ffi_bindings.CXCursor_OMPOrderedDirective -> OMPOrderedDirective
    | Ffi_bindings.CXCursor_OMPAtomicDirective -> OMPAtomicDirective
    | Ffi_bindings.CXCursor_OMPForSimdDirective -> OMPForSimdDirective
    | Ffi_bindings.CXCursor_OMPParallelForSimdDirective -> OMPParallelForSimdDirective
    | Ffi_bindings.CXCursor_OMPTargetDirective -> OMPTargetDirective
    | Ffi_bindings.CXCursor_OMPTeamsDirective -> OMPTeamsDirective
    | Ffi_bindings.CXCursor_OMPTaskgroupDirective -> OMPTaskgroupDirective
    | Ffi_bindings.CXCursor_OMPCancellationPointDirective -> OMPCancellationPointDirective
    | Ffi_bindings.CXCursor_OMPCancelDirective -> OMPCancelDirective
    | Ffi_bindings.CXCursor_OMPTargetDataDirective -> OMPTargetDataDirective
    | Ffi_bindings.CXCursor_OMPTaskLoopDirective -> OMPTaskLoopDirective
    | Ffi_bindings.CXCursor_OMPTaskLoopSimdDirective -> OMPTaskLoopSimdDirective
    | Ffi_bindings.CXCursor_OMPDistributeDirective -> OMPDistributeDirective
    | Ffi_bindings.CXCursor_OMPTargetEnterDataDirective -> OMPTargetEnterDataDirective
    | Ffi_bindings.CXCursor_OMPTargetExitDataDirective -> OMPTargetExitDataDirective
    | Ffi_bindings.CXCursor_OMPTargetParallelDirective -> OMPTargetParallelDirective
    | Ffi_bindings.CXCursor_OMPTargetParallelForDirective -> OMPTargetParallelForDirective
    | Ffi_bindings.CXCursor_OMPTargetUpdateDirective -> OMPTargetUpdateDirective
    | Ffi_bindings.CXCursor_OMPDistributeParallelForDirective -> OMPDistributeParallelForDirective
    | Ffi_bindings.CXCursor_OMPDistributeParallelForSimdDirective -> OMPDistributeParallelForSimdDirective
    | Ffi_bindings.CXCursor_OMPDistributeSimdDirective -> OMPDistributeSimdDirective
    | Ffi_bindings.CXCursor_OMPTargetParallelForSimdDirective -> OMPTargetParallelForSimdDirective
    | Ffi_bindings.CXCursor_LastStmt -> LastStmt
    | Ffi_bindings.CXCursor_TranslationUnit -> TranslationUnit
    | Ffi_bindings.CXCursor_FirstAttr -> FirstAttr
    | Ffi_bindings.CXCursor_UnexposedAttr -> UnexposedAttr
    | Ffi_bindings.CXCursor_IBActionAttr -> IBActionAttr
    | Ffi_bindings.CXCursor_IBOutletAttr -> IBOutletAttr
    | Ffi_bindings.CXCursor_IBOutletCollectionAttr -> IBOutletCollectionAttr
    | Ffi_bindings.CXCursor_CXXFinalAttr -> CXXFinalAttr
    | Ffi_bindings.CXCursor_CXXOverrideAttr -> CXXOverrideAttr
    | Ffi_bindings.CXCursor_AnnotateAttr -> AnnotateAttr
    | Ffi_bindings.CXCursor_AsmLabelAttr -> AsmLabelAttr
    | Ffi_bindings.CXCursor_PackedAttr -> PackedAttr
    | Ffi_bindings.CXCursor_PureAttr -> PureAttr
    | Ffi_bindings.CXCursor_ConstAttr -> ConstAttr
    | Ffi_bindings.CXCursor_NoDuplicateAttr -> NoDuplicateAttr
    | Ffi_bindings.CXCursor_CUDAConstantAttr -> CUDAConstantAttr
    | Ffi_bindings.CXCursor_CUDADeviceAttr -> CUDADeviceAttr
    | Ffi_bindings.CXCursor_CUDAGlobalAttr -> CUDAGlobalAttr
    | Ffi_bindings.CXCursor_CUDAHostAttr -> CUDAHostAttr
    | Ffi_bindings.CXCursor_CUDASharedAttr -> CUDASharedAttr
    | Ffi_bindings.CXCursor_VisibilityAttr -> VisibilityAttr
    | Ffi_bindings.CXCursor_DLLExport -> DLLExport
    | Ffi_bindings.CXCursor_DLLImport -> DLLImport
    | Ffi_bindings.CXCursor_LastAttr -> LastAttr
    | Ffi_bindings.CXCursor_PreprocessingDirective -> PreprocessingDirective
    | Ffi_bindings.CXCursor_MacroDefinition -> MacroDefinition
    | Ffi_bindings.CXCursor_MacroExpansion -> MacroExpansion
    | Ffi_bindings.CXCursor_MacroInstantiation -> MacroInstantiation
    | Ffi_bindings.CXCursor_InclusionDirective -> InclusionDirective
    | Ffi_bindings.CXCursor_FirstPreprocessing -> FirstPreprocessing
    | Ffi_bindings.CXCursor_LastPreprocessing -> LastPreprocessing
    | Ffi_bindings.CXCursor_ModuleImportDecl -> ModuleImportDecl
    | Ffi_bindings.CXCursor_TypeAliasTemplateDecl -> TypeAliasTemplateDecl
    | Ffi_bindings.CXCursor_StaticAssert -> StaticAssert
    | Ffi_bindings.CXCursor_FirstExtraDecl -> FirstExtraDecl
    | Ffi_bindings.CXCursor_LastExtraDecl -> LastExtraDecl
    | Ffi_bindings.CXCursor_OverloadCandidate -> OverloadCandidate


  type t = Ffi_bindings.cx_cursor 

  let get_kind c = kind_of_cx_cursor_kind (B.cx_get_cursor_kind c)
                 
  let cursor_of_translation_unit tu = B.cursor_of_translation_unit_ tu 
      
  let name cursor =
    let i = (B.get_cursor_spelling cursor) in
    let s = B.getcstring_ i in
    B.disposecstring_ i;
    s
    
  let displayname cursor = 
    let i = (B.get_display_name cursor) in
    let s = B.getcstring_ i in
    B.disposecstring_ i;
    s
    
  let cursor_is_null cursor =
    B.cursor_is_null cursor
      
  let coerce_visitor f =
    coerce (Foreign.funptr B.cx_cursor_visitor) (static_funptr B.cx_cursor_visitor) f
      
  let visit cur visitor_func data =
    let data' = ref data in
    let w f data =
      (fun cursor parent clientdata ->
         let visitres, new_data = f cursor parent !data' in
         data' := new_data;
         visit_result visitres 
      )
    in
    let _ = B.visit_children_ cur (coerce_visitor (w visitor_func data')) null in
    !data'
      
  let num_children cur =
    let visitor c p d =
      Continue, d+1
    in
    visit cur visitor 0
      
  let children cur =
    let visitor c p d =
      Continue, List.cons c d
    in
    List.rev (visit cur visitor [])
      
  
  end
 


module Type =
  struct
    type t = cx_type

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


    let kind_to_cx_kind = function 
      | Invalid -> CXTypeInvalid
      | Unexposed -> CXTypeUnexposed
      | Void -> CXTypeVoid
      | Bool -> CXTypeBool
      | Char_U -> CXTypeChar_U
      | UChar -> CXTypeUChar
      | Char16 -> CXTypeChar16
      | Char32 -> CXTypeChar32
      | UShort -> CXTypeUShort
      | UInt -> CXTypeUInt
      | ULong -> CXTypeULong
      | ULongLong -> CXTypeULongLong
      | UInt128 -> CXTypeUInt128
      | Char_S -> CXTypeChar_S
      | SChar -> CXTypeSChar
      | WChar -> CXTypeWChar
      | Short -> CXTypeShort
      | Int -> CXTypeInt
      | Long -> CXTypeLong
      | LongLong -> CXTypeLongLong
      | Int128 -> CXTypeInt128
      | Float -> CXTypeFloat
      | Double -> CXTypeDouble
      | LongDouble -> CXTypeLongDouble
      | NullPtr -> CXTypeNullPtr
      | Overload -> CXTypeOverload
      | Dependent -> CXTypeDependent
      | ObjCId -> CXTypeObjCId
      | ObjCClass -> CXTypeObjCClass
      | ObjCSel -> CXTypeObjCSel
      | FirstBuiltin -> CXTypeFirstBuiltin
      | LastBuiltin -> CXTypeLastBuiltin
      | Complex -> CXTypeComplex
      | Pointer -> CXTypePointer
      | BlockPointer -> CXTypeBlockPointer
      | LValueReference -> CXTypeLValueReference
      | RValueReference -> CXTypeRValueReference
      | Record -> CXTypeRecord
      | Enum -> CXTypeEnum
      | Typedef -> CXTypeTypedef
      | ObjCInterface -> CXTypeObjCInterface
      | ObjCObjectPointer -> CXTypeObjCObjectPointer
      | FunctionNoProto -> CXTypeFunctionNoProto
      | FunctionProto -> CXTypeFunctionProto
      | ConstantArray -> CXTypeConstantArray
      | Vector -> CXTypeVector
      | IncompleteArray -> CXTypeIncompleteArray
      | VariableArray -> CXTypeVariableArray
      | DependentSizedArray -> CXTypeDependentSizedArray
      | MemberPointer -> CXTypeMemberPointer
      | Auto -> CXTypeAuto
      | Elaborated -> CXTypeElaborated

    let cx_kind_to_kind = function 
      | CXTypeInvalid -> Invalid
      | CXTypeUnexposed -> Unexposed
      | CXTypeVoid -> Void
      | CXTypeBool -> Bool
      | CXTypeChar_U -> Char_U
      | CXTypeUChar -> UChar
      | CXTypeChar16 -> Char16
      | CXTypeChar32 -> Char32
      | CXTypeUShort -> UShort
      | CXTypeUInt -> UInt
      | CXTypeULong -> ULong
      | CXTypeULongLong -> ULongLong
      | CXTypeUInt128 -> UInt128
      | CXTypeChar_S -> Char_S
      | CXTypeSChar -> SChar
      | CXTypeWChar -> WChar
      | CXTypeShort -> Short
      | CXTypeInt -> Int
      | CXTypeLong -> Long
      | CXTypeLongLong -> LongLong
      | CXTypeInt128 -> Int128
      | CXTypeFloat -> Float
      | CXTypeDouble -> Double
      | CXTypeLongDouble -> LongDouble
      | CXTypeNullPtr -> NullPtr
      | CXTypeOverload -> Overload
      | CXTypeDependent -> Dependent
      | CXTypeObjCId -> ObjCId
      | CXTypeObjCClass -> ObjCClass
      | CXTypeObjCSel -> ObjCSel
      | CXTypeFirstBuiltin -> FirstBuiltin
      | CXTypeLastBuiltin -> LastBuiltin
      | CXTypeComplex -> Complex
      | CXTypePointer -> Pointer
      | CXTypeBlockPointer -> BlockPointer
      | CXTypeLValueReference -> LValueReference
      | CXTypeRValueReference -> RValueReference
      | CXTypeRecord -> Record
      | CXTypeEnum -> Enum
      | CXTypeTypedef -> Typedef
      | CXTypeObjCInterface -> ObjCInterface
      | CXTypeObjCObjectPointer -> ObjCObjectPointer
      | CXTypeFunctionNoProto -> FunctionNoProto
      | CXTypeFunctionProto -> FunctionProto
      | CXTypeConstantArray -> ConstantArray
      | CXTypeVector -> Vector
      | CXTypeIncompleteArray -> IncompleteArray
      | CXTypeVariableArray -> VariableArray
      | CXTypeDependentSizedArray -> DependentSizedArray
      | CXTypeMemberPointer -> MemberPointer
      | CXTypeAuto -> Auto
      | CXTypeElaborated -> Elaborated


    type calling_conv = Ffi_bindings.calling_conv
    type layout_error = Ffi_bindings.layout_error

    let name t =
      let i = B.get_type_spelling t in
      let s = B.getcstring_ i in
      B.disposecstring_ i;
      s
    
    let kind t =
      cx_kind_to_kind (getf t B.E.cx_type_kind)

    let resolve_typedef c =
      (B.clang_cxtype_resolve_typedef c)

    let get_type_def c =
      B.clang_gettypedef c

    let get_result_type t =
      B.clang_getresulttype t

    let of_cursor c = (B.clang_of_cursor c)

    let get_pointee_type t = B.clang_get_pointee_type t

    let get_bit_width t =
      let i = B.clang_get_field_bit_width t in
      if (i = -1) then None
      else Some i

    let canonical t =
      B.clang_get_canonical_type t



  end


module Index =
  struct
    type t = Ffi_bindings.cx_index

    let create_index exclude_decls_from_pch display_diagnostics =
      B.create_index_ (bool_val exclude_decls_from_pch) (bool_val display_diagnostics)

  end


module TranslationIndex =
  struct
    type t = Ffi_bindings.cx_translation_unit

    type options =
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

    let options_of_translation_unit_options = function
      | Ffi_bindings.CXTranslationUnit_NoOptions -> NoOptions
      | Ffi_bindings.CXTranslationUnit_DetailedPreprocessingRecord -> DetailedPreprocessingRecord
      | Ffi_bindings.CXTranslationUnit_Incomplete -> Incomplete
      | Ffi_bindings.CXTranslationUnit_PrecompiledPreamble -> PrecompiledPreamble
      | Ffi_bindings.CXTranslationUnit_CacheCompletionResults -> CacheCompletionResults
      | Ffi_bindings.CXTranslationUnit_ForSerialization -> ForSerialization
      | Ffi_bindings.CXTranslationUnit_CXXChainedPCH -> CXXChainedPCH
      | Ffi_bindings.CXTranslationUnit_SkipFunctionBodies -> SkipFunctionBodies
      | Ffi_bindings.CXTranslationUnit_IncludeBriefCommentsInCodeCompletion -> IncludeBriefCommentsInCodeCompletion
      | Ffi_bindings.CXTranslationUnit_CreatePreambleOnFirstParse -> CreatePreambleOnFirstParse
      | Ffi_bindings.CXTranslationUnit_KeepGoing -> KeepGoing

    let create_translation_unit_from_source ?(iscpp=false) index file compiler_options =
      let compiler_options =
        if (iscpp) then
          ["-xc++"]@compiler_options
        else
          compiler_options
      in
      let n = List.length compiler_options in
      let args = CArray.start (CArray.of_list string compiler_options) in
      B.create_translation_unit_from_source_ index file n args 0 null

    let get_tu_spelling tu =
      let i = (B.get_tu_spelling tu) in
      let s = B.getcstring_ i in
      B.disposecstring_ i;
      s
  end

 
