use nom::{IResult, Needed, double_s, digit, ErrorKind};

mod ast;

use self::ast::*;

macro_rules! conv {
    ($i:expr, $t:tt :: $t2:tt($($e2:tt)*)) => (do_parse!($i, r: $($e2)* >> ($t :: $t2(r))));
    ($i:expr, $t:tt($($e2:tt)*)) => (do_parse!($i, r: $($e2)* >> ($t(r))));
}

macro_rules! keyword {
    ($i:expr, $e:expr) => (keyword($i, $e))
}

macro_rules! binary_op {
    ($name:ident, [ $op1:tt $(, $ops:tt)* ], $next:ident) => (
        fn $name<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> {
            do_parse!(i,
                first: $next >>
                fold: fold_many0!(
                    do_parse!(
                        operator: alt!(
                            keyword!($op1)
                            $(
                                | 
                                keyword!($ops)
                            )*
                        ) >>
                        expr: $next >>
                        ((operator, expr))
                    ),
                    first,
                    |acc: Expression<'a>, item: (&'a str, Expression<'a>)| {
                        Expression::BinaryExpression(BinaryExpression {
                            left: Box::new(acc),
                            operator: item.0,
                            right: Box::new(item.1),
                        })
                    }
                ) >>
                (fold)
            )
        }
    );
    ($name:ident, $op:tt, $next:ident) => (binary_op!($name, [$op], $next););
}

macro_rules! variable_declaration_list {
    ($i:expr, $k:expr) => (variable_declaration_list($i, $k))
}

macro_rules! variable_declaration {
    ($i:expr, $k:expr) => (variable_declaration($i, $k))
}

macro_rules! variable_declaration_list_not_in {
    ($i:expr, $k:expr) => (variable_declaration_list_not_in($i, $k))
}

macro_rules! variable_declaration_not_in {
    ($i:expr, $k:expr) => (variable_declaration_not_in($i, $k))
}

fn box_opt<T>(i: Option<T>) -> Option<Box<T>> {
    match i {
        Some(v) => Some(Box::new(v)),
        None => None,
    }
}

fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

fn keyword<'a>(i: &'a str, kw: &str) -> IResult<&'a str, &'a str> {
    do_parse!(i,
        take_while_s!(is_whitespace) >>
        ret: tag_s!(kw) >>
        (ret)
    )
}

pub fn parse(i: &str) -> Result<TopLevel, ErrorKind> {
    let res = do_parse!(i,
        ret: top_level >>
        take_while_s!(is_whitespace) >>
        (ret)
    );
    match res {
        IResult::Done(input, output) => {
            if input.len() != 0 {
                return Err(ErrorKind::Custom(10));
            }

            Ok(output)
        },
        IResult::Incomplete(_) => Err(ErrorKind::Complete),
        IResult::Error(e) => Err(e),
    }
}

named!(top_level<&str, TopLevel>, alt!(
    conv!(TopLevel::UiProgram(ui_program))
    |
    conv!(TopLevel::Statement(statement))
    |
    conv!(TopLevel::ExpressionList(expression_list))
    |
    conv!(TopLevel::SourceElement(source_element))
    |
    conv!(TopLevel::UiObjectMember(ui_object_member))
    |
    conv!(TopLevel::Program(program))
));

named!(program<&str, Program>, conv!(Program(opt!(source_elements))));

named!(ui_array_member_list<&str, UiArrayMemberList>, conv!(UiArrayMemberList(separated_nonempty_list!(
    keyword!(","),
    conv!(UiObjectMember::UiObjectDefinition(ui_object_definition))
))));

named!(ui_object_member_list<&str, UiObjectMemberList>, conv!(UiObjectMemberList(many1!(ui_object_member))));

named!(ui_object_member<&str, UiObjectMember>, alt!(
    conv!(UiObjectMember::UiSourceElement(conv!(UiSourceElement::FunctionDeclaration(function_declaration))))
    |
    conv!(UiObjectMember::UiSourceElement(conv!(UiSourceElement::VariableStatement(variable_statement))))
    |
    do_parse!(
        keyword!("signal") >>
        name: identifier >>
        parameters: opt!(do_parse!(
            keyword!("(") >>
            parameters: opt!(ui_parameter_list) >>
            keyword!(")") >>
            (parameters)
        )) >>
        keyword!(";") >>
        (UiObjectMember::UiPublicMember(UiPublicMember {
            name: name,
            type_modifier: None,
            parameters: match parameters {
                Some(p) => p,
                None => None,
            },
            member_type: None,
            public_member_type: UiPublicMemberType::Signal,
            statement: None,
            is_default_member: false,
            is_readonly_member: false,
            binding: None,
        }))
    )
    |
    do_parse!(
        keyword!("property") >>
        type_modifier: identifier >>
        keyword!("<") >>
        member_type: ui_property_type >>
        keyword!(">") >>
        name: js_identifier >>
        keyword!(":") >>
        keyword!("[") >>
        members: ui_array_member_list >>
        keyword!("]") >>
        (UiObjectMember::UiPublicMember(UiPublicMember {
            name: name,
            type_modifier: Some(type_modifier),
            parameters: None,
            member_type: Some(member_type),
            public_member_type: UiPublicMemberType::Property,
            statement: None,
            is_default_member: false,
            is_readonly_member: false,
            binding: Some(Box::new(UiObjectMember::UiArrayBinding(UiArrayBinding {
                qualified_id: UiQualifiedId(vec![name]),
                members: members,
            }))),
        }))
    )
    |
    do_parse!(
        default: opt!(keyword!("default")) >>
        keyword!("property") >>
        type_modifier: identifier >>
        keyword!("<") >>
        member_type: ui_property_type >>
        keyword!(">") >>
        name: js_identifier >>
        keyword!(";") >>
        (UiObjectMember::UiPublicMember(UiPublicMember {
            name: name,
            type_modifier: Some(type_modifier),
            parameters: None,
            member_type: Some(member_type),
            public_member_type: UiPublicMemberType::Property,
            statement: None,
            is_default_member: default.is_some(),
            is_readonly_member: false,
            binding: None,
        }))
    )
    |
    do_parse!(
        readonly: opt!(keyword!("readonly")) >>
        keyword!("property") >>
        member_type: ui_property_type >>
        name: js_identifier >>
        keyword!(":") >>
        qualified_type_name_id: ui_qualified_id >>
        initializer: ui_object_initializer >>
        (UiObjectMember::UiPublicMember(UiPublicMember {
            name: name,
            type_modifier: None,
            parameters: None,
            member_type: Some(member_type),
            public_member_type: UiPublicMemberType::Property,
            statement: None,
            is_default_member: false,
            is_readonly_member: readonly.is_some(),
            binding: Some(Box::new(UiObjectMember::UiObjectBinding(UiObjectBinding {
                qualified_id: UiQualifiedId(vec![name]),
                qualified_type_name_id: qualified_type_name_id,
                initializer: initializer,
            }))),
        }))
    )
    |
    do_parse!(
        default_readonly: opt!(alt!(
            keyword!("default")
            |
            keyword!("readonly")
        )) >>
        keyword!("property") >>
        member_type: ui_property_type >>
        name: js_identifier >>
        keyword!(":") >>
        statement: ui_script_statement >>
        ({
            let (default, readonly) = match default_readonly {
                Some("default") => (true, false),
                Some("readonly") => (false, true),
                _ => (false, false)
            };
            UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: None,
                parameters: None,
                member_type: Some(member_type),
                public_member_type: UiPublicMemberType::Property,
                statement: Some(Box::new(statement)),
                is_default_member: default,
                is_readonly_member: readonly,
                binding: None,
            })
        })
    )
    |
    do_parse!(
        default: opt!(keyword!("default")) >>
        keyword!("property") >>
        member_type: ui_property_type >>
        name: js_identifier >>
        keyword!(";") >>
        (UiObjectMember::UiPublicMember(UiPublicMember {
            name: name,
            type_modifier: None,
            parameters: None,
            member_type: Some(member_type),
            public_member_type: UiPublicMemberType::Property,
            statement: None,
            is_default_member: default.is_some(),
            is_readonly_member: false,
            binding: None,
        }))
    )
    |
    do_parse!(
        qualified_id: ui_qualified_id >>
        keyword!(":") >>
        statement: ui_script_statement >>
        (UiObjectMember::UiScriptBinding(UiScriptBinding {
            qualified_id: qualified_id,
            statement: Box::new(statement),
        }))
    )
    |
    conv!(UiObjectMember::UiObjectDefinition(ui_object_definition))
    |
    do_parse!(
        qualified_id: ui_qualified_id >>
        keyword!(":") >>
        keyword!("[") >>
        members: ui_array_member_list >>
        keyword!("]") >>
        (UiObjectMember::UiArrayBinding(UiArrayBinding {
            qualified_id: qualified_id,
            members: members,
        }))
    )
    |
    do_parse!(
        qualified_id: ui_qualified_id >>
        keyword!(":") >>
        qualified_type_name_id: ui_qualified_id >>
        initializer: ui_object_initializer >>
        (UiObjectMember::UiObjectBinding(UiObjectBinding {
            qualified_id: qualified_id,
            qualified_type_name_id: qualified_type_name_id,
            initializer: initializer,
        }))
    )
    |
    do_parse!(
        qualified_type_name_id: ui_qualified_id >>
        keyword!("on") >>
        qualified_id: ui_qualified_id >>
        initializer: ui_object_initializer >>
        (UiObjectMember::UiObjectBinding(UiObjectBinding {
            qualified_id: qualified_id,
            qualified_type_name_id: qualified_type_name_id,
            initializer: initializer,
        }))
    )
));

named!(ui_parameter_list<&str, UiParameterList>, conv!(UiParameterList(separated_nonempty_list!(
    keyword!(","),
    do_parse!(
        parameter_type: ui_property_type >>
        name: js_identifier >>
        (UiParameter {
            parameter_type: parameter_type,
            name: name,
        })
    )
))));

named!(ui_script_statement<&str, Statement>, alt!(
    conv!(Statement::Block(block))
    |
    conv!(Statement::EmptyStatement(empty_statement))
    |
    conv!(Statement::ExpressionStatement(expression_statement))
    |
    conv!(Statement::IfStatement(if_statement))
    |
    conv!(Statement::WithStatement(with_statement))
    |
    conv!(Statement::SwitchStatement(switch_statement))
    |
    conv!(Statement::TryStatement(try_statement))
));

named!(ui_property_type<&str, UiQualifiedId>, alt!(
    do_parse!(
        string: alt!(
            keyword!("var")
            |
            reserved_word
            |
            identifier
        ) >>
        (UiQualifiedId(vec![string]))
    )
    |
    conv!(UiQualifiedId(separated_nonempty_list!(
        keyword!("."),
        identifier
    )))
));

named!(reserved_word<&str, &str>, alt!(
    keyword!("const")
    |
    keyword!("super")
    |
    keyword!("import")
));

named!(ui_object_initializer<&str, UiObjectInitializer>, do_parse!(
    keyword!("{") >>
    members: opt!(ui_object_member_list) >>
    keyword!("}") >>
    (UiObjectInitializer {
        members: members
    })
));

named!(ui_object_definition<&str, UiObjectDefinition>, do_parse!(
    qualified_type_name_id: ui_qualified_id >>
    initializer: ui_object_initializer >>
    (UiObjectDefinition {
        qualified_type_name_id: qualified_type_name_id,
        initializer: initializer,
    })
));

fn ui_qualified_id(i: &str) -> IResult<&str, UiQualifiedId> {
    let (new_i, expression) = try_parse!(i, member_expression);
    let mut expr_it = &expression;

    if let Expression::ArrayMemberExpression(ref ame) = *expr_it { //TODO: warning ignored annotation
        expr_it = &ame.base;
    };

    let mut names = vec![];

    while let Expression::FieldMemberExpression(ref fme) = *expr_it {
        names.push(fme.name);
        expr_it = &fme.base;
    }

    if let Expression::IdentifierExpression(ref ie) = *expr_it {
        names.push(ie.0);
        IResult::Done(new_i, UiQualifiedId(names))
    } else {
        IResult::Error(ErrorKind::Custom(0))
    }
}

named!(ui_program<&str, UiProgram>, do_parse!(
    headers: opt!(ui_header_item_list) >>
    members: ui_root_member >>
    (UiProgram {
        headers: headers,
        members: members,
    })
));

named!(ui_root_member<&str, UiRootMember>, do_parse!(
    ui_object_definition: ui_object_definition >>
    (UiRootMember(UiObjectMemberList(vec![UiObjectMember::UiObjectDefinition(ui_object_definition)])))
));

named!(ui_header_item_list<&str, UiHeaderItemList>, conv!(UiHeaderItemList(many1!(
    alt!(
        conv!(UiHeaderItem::UiPragma(ui_pragma))
        |
        conv!(UiHeaderItem::UiImport(ui_import))
    )
))));

named!(ui_pragma<&str, UiPragma>, do_parse!(
    keyword!("pragma") >>
    pragma_type: ui_qualified_pragma_id >>
    keyword!(";") >>
    (UiPragma {
        pragma_type: pragma_type
    })
));

named!(ui_import<&str, UiImport>, do_parse!(
    keyword!("import") >>
    file: ui_import_id >>
    version: opt!(numeric_literal) >>
    import_id: opt!(do_parse!(
        keyword!("as") >>
        import_id: js_identifier >>
        (import_id)
    )) >>
    keyword!(";") >>
    (UiImport {
        file: file,
        version: version,
        import_id: import_id,
    })
));

named!(ui_import_id<&str, UiImportId>, alt!(
    conv!(UiImportId::StringLiteral(string_literal))
    |
    conv!(UiImportId::UiQualifiedId(ui_qualified_id))
));

fn ui_qualified_pragma_id(i: &str) -> IResult<&str, UiQualifiedPragmaId> {
    let (new_i, expression) = try_parse!(i, member_expression);
    if let Expression::IdentifierExpression(ie) = expression {
        IResult::Done(new_i, UiQualifiedPragmaId(ie.0))
    } else {
        IResult::Error(ErrorKind::Custom(1))
    }
}

fn member_expression<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> {
    alt!(i,
        do_parse!(
            keyword!("new") >>
            base: member_expression >>
            keyword!("(") >>
            arguments: opt!(argument_list) >>
            keyword!(")") >>
            (Expression::NewMemberExpression(NewMemberExpression {
                base: Box::new(base),
                arguments: arguments,
            }))
        )
        |
        do_parse!(
            first: alt!(
                primary_expression
                |
                function_expression
            ) >>
            fold: fold_many0!(
                alt!(
                    do_parse!(
                        keyword!("[") >>
                        expression: expression_list >>
                        keyword!("]") >>
                        ((Some(expression), None))
                    )
                    |
                    do_parse!(
                        keyword!(".") >>
                        name: property_identifier >>
                        ((None, Some(name)))
                    )
                ),
                first,
                |acc: Expression<'a>, item: (Option<Expression<'a>>, Option<&'a str>)| {
                    match item {
                        (Some(e), None) => {
                            Expression::ArrayMemberExpression(ArrayMemberExpression {
                                base: Box::new(acc),
                                expression: Box::new(e),
                            })
                        },
                        (None, Some(n)) => {
                            Expression::FieldMemberExpression(FieldMemberExpression {
                                base: Box::new(acc),
                                name: n,
                            })
                        },
                        _ => panic!("Logic is broken"),
                    }
                }
            ) >>
            (fold)
        )
    )
}

named!(function_expression<&str, Expression>, do_parse!(
    keyword!("function") >>
    name: opt!(js_identifier) >>
    keyword!("(") >>
    formals: opt!(formal_parameter_list) >>
    keyword!(")") >>
    keyword!("{") >>
    body: opt!(function_body) >>
    keyword!("}") >>
    (Expression::FunctionExpression(FunctionExpression {
        name: name,
        formals: formals,
        body: body,
    }))
));

named!(function_body<&str, FunctionBody>, conv!(FunctionBody(source_elements)));

named!(source_elements<&str, SourceElements>, conv!(SourceElements(many1!(source_element))));

named!(source_element<&str, SourceElement>, alt!(
    conv!(SourceElement::Statement(statement))
    |
    conv!(SourceElement::FunctionDeclaration(function_declaration))
));

named!(statement<&str, Statement>, alt!(
    conv!(Statement::Block(block))
    |
    conv!(Statement::VariableStatement(variable_statement))
    |
    conv!(Statement::EmptyStatement(empty_statement))
    |
    conv!(Statement::ExpressionStatement(expression_statement))
    |
    conv!(Statement::IfStatement(if_statement))
    |
    conv!(Statement::IterationStatement(iteration_statement))
    |
    conv!(Statement::ContinueStatement(continue_statement))
    |
    conv!(Statement::BreakStatement(break_statement))
    |
    conv!(Statement::ReturnStatement(return_statement))
    |
    conv!(Statement::WithStatement(with_statement))
    |
    conv!(Statement::LabelledStatement(labelled_statement))
    |
    conv!(Statement::SwitchStatement(switch_statement))
    |
    conv!(Statement::ThrowStatement(throw_statement))
    |
    conv!(Statement::TryStatement(try_statement))
    |
    conv!(Statement::DebuggerStatement(debugger_statement))
));

named!(debugger_statement<&str, DebuggerStatement>, do_parse!(
    keyword!("debugger") >>
    keyword!(";") >>
    (DebuggerStatement)
));

named!(catch<&str, Catch>, do_parse!(
    keyword!("catch") >>
    keyword!("(") >>
    name: js_identifier >>
    keyword!(")") >>
    statement: block >>
    (Catch {
        name: name,
        statement: statement,
    })
));

named!(finally<&str, Finally>, do_parse!(
    keyword!("finally") >>
    statement: block >>
    (Finally {
        statement: statement,
    })
));

named!(try_statement<&str, TryStatement>, do_parse!(
    keyword!("try") >>
    statement: conv!(Statement::Block(block)) >>
    catch_finally: alt!(
        do_parse!(
            catch: catch >>
            finally: finally >>
            ((Some(catch), Some(finally)))
        )
        |
        do_parse!(
            catch: catch >>
            ((Some(catch), None))
        )
        |
        do_parse!(
            finally: finally >>
            ((None, Some(finally)))
        )
    ) >>
    (TryStatement {
        statement: Box::new(statement),
        catch: catch_finally.0,
        finally: catch_finally.1,
    })
));

named!(throw_statement<&str, ThrowStatement>, do_parse!(
    keyword!("throw") >>
    expression: expression_list >>
    keyword!(";") >>
    (ThrowStatement(Box::new(expression)))
));

named!(switch_statement<&str, SwitchStatement>, do_parse!(
    keyword!("switch") >>
    keyword!("(") >>
    expression: expression_list >>
    keyword!(")") >>
    block: case_block >>
    (SwitchStatement {
        expression: Box::new(expression),
        block: block,
    })
));

named!(case_block<&str, CaseBlock>, do_parse!(
    keyword!("{") >>
    clauses: opt!(case_clauses) >>
    keyword!("}") >>
    default_and_more_clauses: opt!(do_parse!(
        default_clause: default_clause >>
        more_clauses: opt!(case_clauses) >>
        ((default_clause, more_clauses))
    )) >>
    ({
        let (default_clause, more_clauses) = match default_and_more_clauses {
            Some((d, m)) => (Some(d), m),
            None => (None, None),
        };

        CaseBlock {
            clauses: clauses,
            default_clause: default_clause,
            more_clauses: more_clauses,
        }
    })
));

named!(case_clauses<&str, CaseClauses>, conv!(CaseClauses(many1!(case_clause))));

named!(case_clause<&str, CaseClause>, do_parse!(
    keyword!("case") >>
    expression: expression_list >>
    keyword!(":") >>
    statements: opt!(statement_list) >>
    (CaseClause {
        expression: Box::new(expression),
        statements: statements,
    })
));

named!(default_clause<&str, DefaultClause>, do_parse!(
    keyword!("default") >>
    keyword!(":") >>
    statements: opt!(statement_list) >>
    (DefaultClause {
        statements: statements,
    })
));

named!(labelled_statement<&str, LabelledStatement>, do_parse!(
    label: js_identifier >>
    keyword!(":") >>
    statement: statement >>
    (LabelledStatement {
        label: label,
        statement: Box::new(statement),
    })
));

named!(with_statement<&str, WithStatement>, do_parse!(
    keyword!("with") >>
    keyword!("(") >>
    expression: expression_list >>
    keyword!(")") >>
    statement: statement >>
    (WithStatement {
        expression: Box::new(expression),
        statement: Box::new(statement),
    })
));

named!(return_statement<&str, ReturnStatement>, do_parse!(
    keyword!("return") >>
    expression: opt!(expression_list) >>
    keyword!(";") >>
    (ReturnStatement(box_opt(expression)))
));

named!(break_statement<&str, BreakStatement>, do_parse!(
    keyword!("break") >>
    label: opt!(js_identifier) >>
    keyword!(";") >>
    (BreakStatement(label))
));

named!(continue_statement<&str, ContinueStatement>, do_parse!(
    keyword!("continue") >>
    label: opt!(js_identifier) >>
    keyword!(";") >>
    (ContinueStatement(label))
));

named!(iteration_statement<&str, IterationStatement>, alt!(
    do_parse!(
        keyword!("do") >>
        statement: statement >>
        keyword!("while") >>
        keyword!("(") >>
        expression: expression_list >>
        keyword!(")") >>
        keyword!(";") >>
        (IterationStatement::DoWhileStatement(DoWhileStatement {
            statement: Box::new(statement),
            expression: Box::new(expression),
        }))
    )
    |
    do_parse!(
        keyword!("while") >>
        keyword!("(") >>
        expression: expression_list >>
        keyword!(")") >>
        statement: statement >>
        (IterationStatement::WhileStatement(WhileStatement {
            expression: Box::new(expression),
            statement: Box::new(statement),
        }))
    )
    |
    do_parse!(
        keyword!("for") >>
        keyword!("(") >>
        keyword!("var") >>
        declarations: variable_declaration_list_not_in!(VariableDeclarationKind::Var) >>
        keyword!(";") >>
        condition: opt!(expression_list) >>
        keyword!(";") >>
        expression: opt!(expression_list) >>
        keyword!(")") >>
        statement: statement >>
        (IterationStatement::LocalForStatement(LocalForStatement {
            declarations: declarations,
            condition: box_opt(condition),
            expression: box_opt(expression),
            statement: Box::new(statement),
        }))
    )
    |
    do_parse!(
        keyword!("for") >>
        keyword!("(") >>
        initialiser: opt!(expression_list_not_in) >>
        keyword!(";") >>
        condition: opt!(expression_list) >>
        keyword!(";") >>
        expression: opt!(expression_list) >>
        keyword!(")") >>
        statement: statement >>
        (IterationStatement::ForStatement(ForStatement {
            initialiser: box_opt(initialiser),
            condition: box_opt(condition),
            expression: box_opt(expression),
            statement: Box::new(statement),
        }))
    )
    |
    do_parse!(
        keyword!("for") >>
        keyword!("(") >>
        keyword!("var") >>
        declaration: variable_declaration_not_in!(VariableDeclarationKind::Var) >>
        keyword!("in") >>
        expression: expression_list >>
        keyword!(")") >>
        statement: statement >>
        (IterationStatement::LocalForEachStatement(LocalForEachStatement {
            declaration: declaration,
            expression: Box::new(expression),
            statement: Box::new(statement),
        }))
    )
    |
    do_parse!(
        keyword!("for") >>
        keyword!("(") >>
        initialiser: left_hand_side_expression >>
        keyword!("in") >>
        expression: expression_list >>
        keyword!(")") >>
        statement: statement >>
        (IterationStatement::ForEachStatement(ForEachStatement {
            initialiser: Box::new(initialiser),
            expression: Box::new(expression),
            statement: Box::new(statement),
        }))
    )
));

named!(empty_statement<&str, EmptyStatement>, do_parse!(
    keyword!(";") >>
    (EmptyStatement)
));

named!(expression_statement<&str, ExpressionStatement>, do_parse!(
    expression: expression_list >>
    keyword!(";") >>
    (ExpressionStatement(Box::new(expression)))
));

named!(if_statement<&str, IfStatement>, do_parse!(
    keyword!("if") >>
    keyword!("(") >>
    expression: expression_list >>
    keyword!(")") >>
    ok:statement >>
    ko: opt!(do_parse!(
        keyword!("else") >>
        statement: statement >>
        (Box::new(statement))
    )) >>
    (IfStatement {
        expression: Box::new(expression),
        ok: Box::new(ok),
        ko: ko,
    })
));

named!(block<&str, Block>, do_parse!(
    keyword!("{") >>
    statements: opt!(statement_list) >>
    keyword!("}") >>
    (Block {
        statements: statements
    })
));

named!(statement_list<&str, StatementList>, conv!(StatementList(many1!(statement))));

named!(variable_statement<&str, VariableStatement>, do_parse!(
    kind: variable_declaration_kind >>
    declarations: variable_declaration_list!(kind) >>
    keyword!(";") >>
    (VariableStatement {
        declarations: declarations,
    })
));

named!(variable_declaration_kind<&str, VariableDeclarationKind>, alt!(
    do_parse!(
        keyword!("const") >>
        (VariableDeclarationKind::Const)
    )
    |
    do_parse!(
        keyword!("var") >>
        (VariableDeclarationKind::Var)
    )
));

fn variable_declaration_list(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclarationList> {
    conv!(i, VariableDeclarationList(separated_nonempty_list!(
        keyword!(","),
        variable_declaration!(k.clone())
    )))
}

fn variable_declaration(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclaration> {
    do_parse!(i,
        name: js_identifier >>
        expression: opt!(initialiser) >>
        (VariableDeclaration {
            name: name,
            expression: {
                match expression {
                    Some(v) => Some(Box::new(v)),
                    None => None
                }
            },
            kind: k,
        })
    )
}

fn variable_declaration_list_not_in(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclarationList> {
    conv!(i, VariableDeclarationList(separated_nonempty_list!(
        keyword!(","),
        variable_declaration_not_in!(k.clone())
    )))
}

fn variable_declaration_not_in(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclaration> {
    do_parse!(i,
        name: js_identifier >>
        expression: opt!(initialiser_not_in) >>
        (VariableDeclaration {
            name: name,
            expression: {
                match expression {
                    Some(v) => Some(Box::new(v)),
                    None => None
                }
            },
            kind: k,
        })
    )
}

named!(initialiser<&str, Expression>, do_parse!(
    keyword!("=") >>
    expression: assignment_expression >>
    (expression)
));

named!(initialiser_not_in<&str, Expression>, do_parse!(
    keyword!("=") >>
    expression: assignment_expression_not_in >>
    (expression)
));

named!(primary_expression<&str, Expression>, alt!(
    do_parse!(
        keyword!("this") >>
        (Expression::ThisExpression(ThisExpression))
    )
    |
    do_parse!(
        keyword!("null") >>
        (Expression::NullExpression(NullExpression))
    )
    |
    do_parse!(
        keyword!("true") >>
        (Expression::TrueLiteral(TrueLiteral))
    )
    |
    do_parse!(
        keyword!("false") >>
        (Expression::FalseLiteral(FalseLiteral))
    )
    |
    do_parse!(
        keyword!("[") >>
        elements: opt!(do_parse!(
            element_list: element_list >>
            opt!(keyword!(",")) >>
            (element_list)
        )) >>
        elision: opt!(elision) >>
        keyword!("]") >>
        (Expression::ArrayLiteral(ArrayLiteral {
            elements: elements,
            elision: elision,
        }))
    )
    |
    do_parse!(
        keyword!("{") >>
        properties: opt!(do_parse!(
            property_assignment_list: property_assignment_list >>
            opt!(keyword!(",")) >>
            (property_assignment_list)
        )) >>
        keyword!("}") >>
        (Expression::ObjectLiteral(ObjectLiteral {
            properties: properties,
        }))
    )
    |
    do_parse!(
        keyword!("(") >>
        expression: expression_list >>
        keyword!(")") >>
        (Expression::NestedExpression(NestedExpression(Box::new(expression))))
    )
    |
    conv!(Expression::NumericLiteral(numeric_literal))
    |
    conv!(Expression::StringLiteral(string_literal))
    |
    conv!(Expression::IdentifierExpression(conv!(IdentifierExpression(js_identifier))))
));

named!(property_assignment_list<&str, PropertyAssignmentList>, conv!(PropertyAssignmentList(separated_nonempty_list!(
    keyword!(","),
    property_assignment
))));

named!(property_assignment<&str, PropertyAssignment>, alt!(
    do_parse!(
        keyword!("get") >>
        name: property_name >>
        keyword!("(") >>
        keyword!(")") >>
        keyword!("{") >>
        function_body: opt!(function_body) >>
        keyword!("}") >>
        (PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
            name: name,
            getter_setter_type: PropertyGetterSetterType::Getter,
            formals: None,
            function_body: function_body,
        }))
    )
    |
    do_parse!(
        keyword!("set") >>
        name: property_name >>
        keyword!("(") >>
        formals: opt!(formal_parameter_list) >>
        keyword!(")") >>
        keyword!("{") >>
        function_body: opt!(function_body) >>
        keyword!("}") >>
        (PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
            name: name,
            getter_setter_type: PropertyGetterSetterType::Setter,
            formals: formals,
            function_body: function_body,
        }))
    )
    |
    do_parse!(
        name: property_name >>
        keyword!(":") >>
        value: assignment_expression >>
        (PropertyAssignment::PropertyNameAndValue(PropertyNameAndValue {
            name: name,
            value: value,
        }))
    )
));

named!(property_name<&str, PropertyName>, alt!(
    conv!(PropertyName::IdentifierPropertyName(conv!(IdentifierPropertyName(reserved_identifier))))
    |
    conv!(PropertyName::IdentifierPropertyName(conv!(IdentifierPropertyName(js_identifier))))
    |
    conv!(PropertyName::StringLiteralPropertyName(conv!(StringLiteralPropertyName(string_literal))))
    |
    conv!(PropertyName::NumericLiteralPropertyName(conv!(NumericLiteralPropertyName(numeric_literal))))
));

named!(function_declaration<&str, FunctionDeclaration>, do_parse!(
    keyword!("function") >>
    name: js_identifier >>
    keyword!("(") >>
    formal_parameter_list: opt!(formal_parameter_list) >>
    keyword!(")") >>
    keyword!("{") >>
    body: opt!(function_body) >>
    keyword!("}") >>
    (FunctionDeclaration {
        name: name,
        formals: formal_parameter_list,
        body: body,
    })
));

named!(numeric_literal<&str, NumericLiteral>, conv!(NumericLiteral(do_parse!(
    take_while_s!(is_whitespace) >>
    numeric_literal: alt!(
        double_s
        |
        do_parse!(
            int: digit >>
            (int.parse::<f64>().unwrap())
        )
    ) >>
    (numeric_literal)
))));

named!(string_literal<&str, StringLiteral>, do_parse!(
    delimiter: alt!(
        keyword!("\"")
        |
        keyword!("'")
    ) >>
    string: take_until_s!(delimiter) >> // TODO: escape chars
    keyword!(delimiter) >>
    (StringLiteral(string))
));

named!(js_identifier<&str, &str>, alt!(
    keyword!("property")
    |
    keyword!("signal")
    |
    keyword!("readonly")
    |
    keyword!("on")
    |
    keyword!("get")
    |
    keyword!("set")
    |
    identifier
));

named!(identifier<&str, &str>, do_parse!(
    take_while_s!(is_whitespace) >>
    identifier: recognize!(do_parse!(
        xid_start >>
        many0!(xid_continue) >>
        ()
    )) >>
    (identifier)
));

named!(xid_start<&str, char>, verify!(
    anychar_s,
    |val:char| val.is_xid_start()
));

named!(xid_continue<&str, char>, verify!(
    anychar_s,
    |val:char| val.is_xid_continue()
));

pub fn anychar_s(input:&str) -> IResult<&str, char> {
  if input.is_empty() {
    IResult::Incomplete(Needed::Size(1))
  } else {
    IResult::Done(&input[1..], input.chars().nth(0).unwrap())
  }
}

named!(element_list<&str, ElementList>, conv!(ElementList(separated_nonempty_list!(
    keyword!(","),
    do_parse!(
        elision_opt: opt!(elision) >>
        assignment_expression: assignment_expression >>
        (Element {
            elision: elision_opt,
            expression: assignment_expression,
        })
    )
))));

named!(elision<&str, Elision>, do_parse!(
    vec: many1!(keyword!(",")) >>
    (Elision(vec.len()))
));

named!(formal_parameter_list<&str, FormalParameterList>, conv!(FormalParameterList(separated_nonempty_list!(
    keyword!(","),
    conv!(FormalParameter(js_identifier))
))));

named!(assignment_expression<&str, Expression>, alt!(
    do_parse!(
        left: left_hand_side_expression >>
        operator: assignment_operator >>
        right: assignment_expression >>
        (Expression::BinaryExpression(BinaryExpression {
            left: Box::new(left),
            operator: operator,
            right: Box::new(right),
        }))
    )
    |
    conditional_expression
));

named!(assignment_expression_not_in<&str, Expression>, alt!(
    do_parse!(
        left: left_hand_side_expression >>
        operator: assignment_operator >>
        right: assignment_expression_not_in >>
        (Expression::BinaryExpression(BinaryExpression {
            left: Box::new(left),
            operator: operator,
            right: Box::new(right),
        }))
    )
    |
    conditional_expression_not_in
));

named!(assignment_operator<&str, &str>, alt!(
    keyword!("=")
    |
    keyword!("*=")
    |
    keyword!("/=")
    |
    keyword!("%=")
    |
    keyword!("+=")
    |
    keyword!("-=")
    |
    keyword!("<<=")
    |
    keyword!(">>>=")
    |
    keyword!(">>=")
    |
    keyword!("&=")
    |
    keyword!("^=")
    |
    keyword!("|=")
));

named!(conditional_expression<&str, Expression>, alt!(
    do_parse!(
        expression: logical_or_expression >>
        keyword!("?") >>
        ok: assignment_expression >>
        keyword!(":") >>
        ko: assignment_expression >>
        (Expression::ConditionalExpression(ConditionalExpression {
            expression: Box::new(expression),
            ok: Box::new(ok),
            ko: Box::new(ko),
        }))
    )
    |
    logical_or_expression
));

named!(conditional_expression_not_in<&str, Expression>, alt!(
    do_parse!(
        expression: logical_or_expression_not_in >>
        keyword!("?") >>
        ok: assignment_expression_not_in >>
        keyword!(":") >>
        ko: assignment_expression_not_in >>
        (Expression::ConditionalExpression(ConditionalExpression {
            expression: Box::new(expression),
            ok: Box::new(ok),
            ko: Box::new(ko),
        }))
    )
    |
    logical_or_expression
));

binary_op!(logical_or_expression,"||", logical_and_expression);
binary_op!(logical_and_expression, "&&", bitwise_or_expression);
binary_op!(bitwise_or_expression, "|", bitwise_xor_expression);
binary_op!(bitwise_xor_expression, "^", bitwise_and_expression);
binary_op!(bitwise_and_expression, "&", equality_expression);
binary_op!(equality_expression, ["===", "!==", "==", "!="], relational_expression);
binary_op!(relational_expression, [">=", "<=", ">", "<", "instanceof", "in"], shift_expression);

binary_op!(logical_or_expression_not_in, "||", logical_and_expression_not_in); // TODO: fix when macros are allowed as idents
binary_op!(logical_and_expression_not_in, "&&", bitwise_or_expression_not_in);
binary_op!(bitwise_or_expression_not_in, "|", bitwise_xor_expression_not_in);
binary_op!(bitwise_xor_expression_not_in, "^", bitwise_and_expression_not_in);
binary_op!(bitwise_and_expression_not_in, "&", equality_expression_not_in);
binary_op!(equality_expression_not_in, ["===", "!==", "==", "!="], relational_expression_not_in);
binary_op!(relational_expression_not_in, [">=", "<=", ">", "<", "instanceof"], shift_expression);

binary_op!(shift_expression, [">>>", ">>", "<<"], additive_expression);
binary_op!(additive_expression, ["+", "-"], multiplicative_expression);
binary_op!(multiplicative_expression, ["*", "/", "%"], unary_expression);

named!(unary_expression<&str, Expression>, alt!(
    do_parse!(
        keyword!("delete") >>
        expression: unary_expression >>
        (Expression::DeleteExpression(DeleteExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("void") >>
        expression: unary_expression >>
        (Expression::VoidExpression(VoidExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("typeof") >>
        expression: unary_expression >>
        (Expression::TypeOfExpression(TypeOfExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("++") >>
        expression: unary_expression >>
        (Expression::PreIncrementExpression(PreIncrementExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("--") >>
        expression: unary_expression >>
        (Expression::PreDecrementExpression(PreDecrementExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("+") >>
        expression: unary_expression >>
        (Expression::UnaryPlusExpression(UnaryPlusExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("-") >>
        expression: unary_expression >>
        (Expression::UnaryMinusExpression(UnaryMinusExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("~") >>
        expression: unary_expression >>
        (Expression::TildeExpression(TildeExpression(Box::new(expression))))
    )
    |
    do_parse!(
        keyword!("!") >>
        expression: unary_expression >>
        (Expression::NotExpression(NotExpression(Box::new(expression))))
    )
    |
    postfix_expression
));

named!(postfix_expression<&str, Expression>, alt!(
    do_parse!(
        expression: left_hand_side_expression >>
        keyword!("++") >>
        (Expression::PostIncrementExpression(PostIncrementExpression(Box::new(expression))))
    )
    |
    do_parse!(
        expression: left_hand_side_expression >>
        keyword!("--") >>
        (Expression::PostDecrementExpression(PostDecrementExpression(Box::new(expression))))
    )
    |
    left_hand_side_expression
));

named!(left_hand_side_expression<&str, Expression>, alt!(
    new_expression
    |
    call_expression
));

named!(new_expression<&str, Expression>, alt!(
    do_parse!(
        keyword!("new") >>
        expression: new_expression >>
        (Expression::NewExpression(NewExpression(Box::new(expression))))
    )
    |
    member_expression
));

named!(call_expression<&str, Expression>, alt!(
    do_parse!(
        base: alt!(member_expression | call_expression) >>
        keyword!("(") >>
        arguments: opt!(argument_list) >>
        keyword!(")") >>
        (Expression::CallExpression(CallExpression {
            base: Box::new(base),
            arguments: arguments,
        }))
    )
    |
    do_parse!(
        base: call_expression >>
        keyword!("[") >>
        expression: expression_list >>
        keyword!("]") >>
        (Expression::ArrayMemberExpression(ArrayMemberExpression {
            base: Box::new(base),
            expression: Box::new(expression),
        }))
    )
    |
    do_parse!(
        base: call_expression >>
        keyword!(".") >>
        name: property_identifier >>
        (Expression::FieldMemberExpression(FieldMemberExpression {
            base: Box::new(base),
            name: name,
        }))
    )
));

named!(argument_list<&str, ArgumentList>, conv!(ArgumentList(separated_nonempty_list!(
    keyword!(","),
    assignment_expression
))));

fn expression_list<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> { // called Expression in qqmljs.g
    do_parse!(i,
        first: assignment_expression >>
        fold: fold_many0!(
            do_parse!(
                keyword!(",") >>
                expr: assignment_expression >>
                (expr)
            ),
            first,
            |acc: Expression<'a>, item: Expression<'a>| {
                Expression::ExpressionList(ExpressionList {
                    left: Box::new(acc),
                    right: Box::new(item),
                })
            }
        ) >>
        (fold)
    )
}

fn expression_list_not_in<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> { // called Expression in qqmljs.g
    do_parse!(i,
        first: assignment_expression_not_in >>
        fold: fold_many0!(
            assignment_expression_not_in,
            first,
            |acc: Expression<'a>, item: Expression<'a>| {
                Expression::ExpressionList(ExpressionList {
                    left: Box::new(acc),
                    right: Box::new(item),
                })
            }
        ) >>
        (fold)
    )
}

named!(property_identifier<&str, &str>, alt!(
    reserved_identifier
    |
    js_identifier
));

named!(reserved_identifier<&str, &str>, alt!(
    keyword!("break")
    |
    keyword!("case")
    |
    keyword!("catch")
    |
    keyword!("continue")
    |
    keyword!("default")
    |
    keyword!("delete")
    |
    keyword!("do")
    |
    keyword!("else")
    |
    keyword!("false")
    |
    keyword!("finally")
    |
    keyword!("for")
    |
    keyword!("function")
    |
    keyword!("if")
    |
    keyword!("in")
    |
    keyword!("instanceof")
    |
    keyword!("new")
    |
    keyword!("null")
    |
    keyword!("return")
    |
    keyword!("switch")
    |
    keyword!("this")
    |
    keyword!("throw")
    |
    keyword!("true")
    |
    keyword!("try")
    |
    keyword!("typeof")
    |
    keyword!("var")
    |
    keyword!("void")
    |
    keyword!("while")
    |
    keyword!("const")
    |
    keyword!("debugger")
    |
    keyword!("with")
));
