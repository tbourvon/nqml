use std;

#[derive(Debug)]
pub enum TopLevel<'a> {
    UiProgram(UiProgram<'a>),
    Statement(Statement<'a>),
    ExpressionList(Expression<'a>),
    SourceElement(SourceElement<'a>),
    UiObjectMember(UiObjectMember<'a>),
    Program(Program<'a>),
}

#[derive(Debug)]
pub struct Program<'a>(pub Option<SourceElements<'a>>);

#[derive(Debug)]
pub enum UiObjectMember<'a> {
    UiObjectDefinition(UiObjectDefinition<'a>),
    UiArrayBinding(UiArrayBinding<'a>),
    UiObjectBinding(UiObjectBinding<'a>),
    UiPublicMember(UiPublicMember<'a>),
    UiScriptBinding(UiScriptBinding<'a>),
    UiSourceElement(UiSourceElement<'a>),
}

#[derive(Debug)]
pub struct UiParameterList<'a>(pub std::vec::Vec<UiParameter<'a>>);

#[derive(Debug)]
pub struct UiParameter<'a> {
    pub parameter_type: UiQualifiedId<'a>,
    pub name: &'a str,
}

#[derive(Debug)]
pub struct UiScriptBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub enum UiSourceElement<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableStatement(VariableStatement<'a>),
}

#[derive(Debug)]
pub enum UiPublicMemberType {
    Signal,
    Property,
}

#[derive(Debug)]
pub struct UiPublicMember<'a> {
    pub public_member_type: UiPublicMemberType,
    pub type_modifier: Option<&'a str>,
    pub member_type: Option<UiQualifiedId<'a>>,
    pub name: &'a str,
    pub statement: Option<Box<Statement<'a>>>,
    pub binding: Option<Box<UiObjectMember<'a>>>,
    pub is_default_member: bool,
    pub is_readonly_member: bool,
    pub parameters: Option<UiParameterList<'a>>,
}

#[derive(Debug)]
pub struct UiObjectDefinition<'a> {
    pub qualified_type_name_id: UiQualifiedId<'a>,
    pub initializer: UiObjectInitializer<'a>,
}

#[derive(Debug)]
pub struct UiArrayMemberList<'a>(pub std::vec::Vec<UiObjectMember<'a>>);

#[derive(Debug)]
pub struct UiArrayBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub members: UiArrayMemberList<'a>,
}

#[derive(Debug)]
pub struct UiObjectBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub qualified_type_name_id: UiQualifiedId<'a>,
    pub initializer: UiObjectInitializer<'a>,
}

#[derive(Debug)]
pub struct UiQualifiedId<'a>(pub std::vec::Vec<&'a str>);

#[derive(Debug)]
pub enum Statement<'a> {
    Block(Block<'a>),
    VariableStatement(VariableStatement<'a>),
    EmptyStatement(EmptyStatement),
    ExpressionStatement(ExpressionStatement<'a>),
    IfStatement(IfStatement<'a>),
    IterationStatement(IterationStatement<'a>),
    ContinueStatement(ContinueStatement<'a>),
    BreakStatement(BreakStatement<'a>),
    ReturnStatement(ReturnStatement<'a>),
    WithStatement(WithStatement<'a>),
    LabelledStatement(LabelledStatement<'a>),
    SwitchStatement(SwitchStatement<'a>),
    ThrowStatement(ThrowStatement<'a>),
    TryStatement(TryStatement<'a>),
    DebuggerStatement(DebuggerStatement),
}

#[derive(Debug)]
pub struct UiObjectInitializer<'a> {
    pub members: Option<UiObjectMemberList<'a>>,
}

#[derive(Debug)]
pub struct UiObjectMemberList<'a>(pub std::vec::Vec<UiObjectMember<'a>>);

#[derive(Debug)]
pub struct DebuggerStatement;

#[derive(Debug)]
pub struct Catch<'a> {
    pub name: &'a str,
    pub statement: Block<'a>,
}

#[derive(Debug)]
pub struct Finally<'a> {
    pub statement: Block<'a>,
}

#[derive(Debug)]
pub struct TryStatement<'a> {
    pub statement: Box<Statement<'a>>,
    pub catch: Option<Catch<'a>>,
    pub finally: Option<Finally<'a>>,
}

#[derive(Debug)]
pub struct ThrowStatement<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct CaseClause<'a> {
    pub expression: Box<Expression<'a>>,
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug)]
pub struct DefaultClause<'a> {
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug)]
pub struct CaseClauses<'a>(pub std::vec::Vec<CaseClause<'a>>);

#[derive(Debug)]
pub struct CaseBlock<'a> {
    pub clauses: Option<CaseClauses<'a>>,
    pub default_clause: Option<DefaultClause<'a>>,
    pub more_clauses: Option<CaseClauses<'a>>,
}

#[derive(Debug)]
pub struct SwitchStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub block: CaseBlock<'a>,
}

#[derive(Debug)]
pub enum IterationStatement<'a> {
    DoWhileStatement(DoWhileStatement<'a>),
    WhileStatement(WhileStatement<'a>),
    ForStatement(ForStatement<'a>),
    LocalForStatement(LocalForStatement<'a>),
    ForEachStatement(ForEachStatement<'a>),
    LocalForEachStatement(LocalForEachStatement<'a>),
}

#[derive(Debug)]
pub struct LabelledStatement<'a> {
    pub label: &'a str,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct WithStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct ReturnStatement<'a>(pub Option<Box<Expression<'a>>>);

#[derive(Debug)]
pub struct BreakStatement<'a>(pub Option<&'a str>);

#[derive(Debug)]
pub struct ContinueStatement<'a>(pub Option<&'a str>);

#[derive(Debug)]
pub struct LocalForEachStatement<'a> {
    pub declaration: VariableDeclaration<'a>,
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct ForEachStatement<'a> {
    pub initialiser: Box<Expression<'a>>,
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct LocalForStatement<'a> {
    pub declarations: VariableDeclarationList<'a>,
    pub condition: Option<Box<Expression<'a>>>,
    pub expression: Option<Box<Expression<'a>>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct ForStatement<'a> {
    pub initialiser: Option<Box<Expression<'a>>>,
    pub condition: Option<Box<Expression<'a>>>,
    pub expression: Option<Box<Expression<'a>>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub struct DoWhileStatement<'a> {
    pub statement: Box<Statement<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct WhileStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}


#[derive(Debug)]
pub struct IfStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub ok: Box<Statement<'a>>,
    pub ko: Option<Box<Statement<'a>>>,
}

#[derive(Debug)]
pub struct ExpressionStatement<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct EmptyStatement;

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug)]
pub struct VariableStatement<'a> {
    pub declarations: VariableDeclarationList<'a>,
}

#[derive(Clone)]
#[derive(Debug)]
pub enum VariableDeclarationKind {
    Const,
    Var,
}

#[derive(Debug)]
pub struct VariableDeclarationList<'a>(pub std::vec::Vec<VariableDeclaration<'a>>);

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub name: &'a str,
    pub expression: Option<Box<Expression<'a>>>,
    pub kind: VariableDeclarationKind,
}

#[derive(Debug)]
pub struct StatementList<'a>(pub std::vec::Vec<Statement<'a>>);

#[derive(Debug)]
pub struct UiProgram<'a> {
    pub headers: Option<UiHeaderItemList<'a>>,
    pub members: UiRootMember<'a>,
}

#[derive(Debug)]
pub struct UiRootMember<'a>(pub UiObjectMemberList<'a>);

#[derive(Debug)]
pub struct UiHeaderItemList<'a>(pub std::vec::Vec<UiHeaderItem<'a>>);

#[derive(Debug)]
pub enum UiHeaderItem<'a> {
    UiPragma(UiPragma<'a>),
    UiImport(UiImport<'a>),
}

#[derive(Debug)]
pub struct UiImport<'a> {
    pub file: UiImportId<'a>,
    pub import_id: Option<&'a str>,
    pub version: Option<NumericLiteral>,
}

#[derive(Debug)]
pub struct UiPragma<'a> {
    pub pragma_type: UiQualifiedPragmaId<'a>,
}

#[derive(Debug)]
pub struct UiQualifiedPragmaId<'a>(pub &'a str);

#[derive(Debug)]
pub enum UiImportId<'a> {
    UiQualifiedId(UiQualifiedId<'a>),
    StringLiteral(StringLiteral<'a>),
}

#[derive(Debug)]
pub struct ThisExpression;
#[derive(Debug)]
pub struct IdentifierExpression<'a>(pub &'a str);
#[derive(Debug)]
pub struct NullExpression;
#[derive(Debug)]
pub struct TrueLiteral;
#[derive(Debug)]
pub struct FalseLiteral;
#[derive(Debug)]
pub struct NumericLiteral(pub f64);
#[derive(Debug)]
pub struct StringLiteral<'a>(pub &'a str);
#[derive(Debug)]
pub struct ArrayLiteral<'a> {
    pub elements: Option<ElementList<'a>>,
    pub elision: Option<Elision>,
}
#[derive(Debug)]
pub struct ObjectLiteral<'a> {
    pub properties: Option<PropertyAssignmentList<'a>>,
}
#[derive(Debug)]
pub struct NestedExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct ElementList<'a>(pub std::vec::Vec<Element<'a>>);
#[derive(Debug)]
pub struct Element<'a> {
    pub elision: Option<Elision>,
    pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct ArgumentList<'a>(pub std::vec::Vec<Expression<'a>>);

#[derive(Debug)]
pub struct Elision(pub usize);

#[derive(Debug)]
pub enum Expression<'a> {
    CallExpression(CallExpression<'a>),
    ConditionalExpression(ConditionalExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
    NotExpression(NotExpression<'a>),
    TildeExpression(TildeExpression<'a>),
    UnaryMinusExpression(UnaryMinusExpression<'a>),
    UnaryPlusExpression(UnaryPlusExpression<'a>),
    PreDecrementExpression(PreDecrementExpression<'a>),
    PreIncrementExpression(PreIncrementExpression<'a>),
    TypeOfExpression(TypeOfExpression<'a>),
    VoidExpression(VoidExpression<'a>),
    DeleteExpression(DeleteExpression<'a>),
    PostDecrementExpression(PostDecrementExpression<'a>),
    PostIncrementExpression(PostIncrementExpression<'a>),
    FunctionExpression(FunctionExpression<'a>),
    ArrayMemberExpression(ArrayMemberExpression<'a>),
    FieldMemberExpression(FieldMemberExpression<'a>),
    NewMemberExpression(NewMemberExpression<'a>),
    ThisExpression(ThisExpression),
    IdentifierExpression(IdentifierExpression<'a>),
    NullExpression(NullExpression),
    TrueLiteral(TrueLiteral),
    FalseLiteral(FalseLiteral),
    NumericLiteral(NumericLiteral),
    //TODO: Multiline string literal
    StringLiteral(StringLiteral<'a>),
    //TODO: regexp
    ArrayLiteral(ArrayLiteral<'a>),
    ObjectLiteral(ObjectLiteral<'a>),
    NestedExpression(NestedExpression<'a>),
    ExpressionList(ExpressionList<'a>),
    NewExpression(NewExpression<'a>),
}

#[derive(Debug)]
pub struct NotExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct TildeExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct UnaryMinusExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct UnaryPlusExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct PreDecrementExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct PreIncrementExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct TypeOfExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct VoidExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct DeleteExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct PostDecrementExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct PostIncrementExpression<'a>(pub Box<Expression<'a>>);
#[derive(Debug)]
pub struct NewExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct FunctionExpression<'a> {
    pub name: Option<&'a str>,
    pub formals: Option<FormalParameterList<'a>>,
    pub body: Option<FunctionBody<'a>>,
}

#[derive(Debug)]
pub struct NewMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug)]
pub struct CallExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug)]
pub struct ArrayMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct FieldMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub name: &'a str,
}

#[derive(Debug)]
pub struct ExpressionList<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: &'a str,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct ConditionalExpression<'a> {
    pub expression: Box<Expression<'a>>,
    pub ok: Box<Expression<'a>>,
    pub ko: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct PropertyAssignmentList<'a>(pub std::vec::Vec<PropertyAssignment<'a>>);
#[derive(Debug)]
pub enum PropertyAssignment<'a> {
    PropertyNameAndValue(PropertyNameAndValue<'a>),
    PropertyGetterSetter(PropertyGetterSetter<'a>),
}

#[derive(Debug)]
pub struct PropertyNameAndValue<'a> {
    pub name: PropertyName<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug)]
pub struct PropertyGetterSetter<'a> {
    pub name: PropertyName<'a>,
    pub getter_setter_type: PropertyGetterSetterType,
    pub formals: Option<FormalParameterList<'a>>,
    pub function_body: Option<FunctionBody<'a>>,
}

#[derive(Debug)]
pub enum PropertyGetterSetterType {
    Getter,
    Setter,
}

#[derive(Debug)]
pub enum PropertyName<'a> {
    IdentifierPropertyName(IdentifierPropertyName<'a>),
    StringLiteralPropertyName(StringLiteralPropertyName<'a>),
    NumericLiteralPropertyName(NumericLiteralPropertyName),
}

#[derive(Debug)]
pub struct IdentifierPropertyName<'a>(pub &'a str);
#[derive(Debug)]
pub struct StringLiteralPropertyName<'a>(pub StringLiteral<'a>);
#[derive(Debug)]
pub struct NumericLiteralPropertyName(pub NumericLiteral);

#[derive(Debug)]
pub struct FormalParameterList<'a>(pub std::vec::Vec<FormalParameter<'a>>);
#[derive(Debug)]
pub struct FormalParameter<'a>(pub &'a str);

#[derive(Debug)]
pub struct FunctionBody<'a>(pub SourceElements<'a>);

#[derive(Debug)]
pub struct SourceElements<'a>(pub std::vec::Vec<SourceElement<'a>>);

#[derive(Debug)]
pub enum SourceElement<'a> {
    Statement(Statement<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub formals: Option<FormalParameterList<'a>>,
    pub body: Option<FunctionBody<'a>>,
}