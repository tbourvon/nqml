use std;
use parser::js::expressions::*;

#[derive(Debug, PartialEq)]
pub struct StatementList<'a>(pub std::vec::Vec<Statement<'a>>);

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Block<'a> {
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct VariableStatement<'a> {
    pub declarations: VariableDeclarationList<'a>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarationList<'a>(pub std::vec::Vec<VariableDeclaration<'a>>);

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration<'a> {
    pub name: &'a str,
    pub expression: Option<Box<Expression<'a>>>,
    pub kind: VariableDeclarationKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariableDeclarationKind {
    Const,
    Var,
}

#[derive(Debug, PartialEq)]
pub struct EmptyStatement;

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct IfStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub ok: Box<Statement<'a>>,
    pub ko: Option<Box<Statement<'a>>>,
}

#[derive(Debug, PartialEq)]
pub enum IterationStatement<'a> {
    DoWhileStatement(DoWhileStatement<'a>),
    WhileStatement(WhileStatement<'a>),
    ForStatement(ForStatement<'a>),
    LocalForStatement(LocalForStatement<'a>),
    ForEachStatement(ForEachStatement<'a>),
    LocalForEachStatement(LocalForEachStatement<'a>),
}

#[derive(Debug, PartialEq)]
pub struct DoWhileStatement<'a> {
    pub statement: Box<Statement<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ForStatement<'a> {
    pub initialiser: Option<Box<Expression<'a>>>,
    pub condition: Option<Box<Expression<'a>>>,
    pub expression: Option<Box<Expression<'a>>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct LocalForStatement<'a> {
    pub declarations: VariableDeclarationList<'a>,
    pub condition: Option<Box<Expression<'a>>>,
    pub expression: Option<Box<Expression<'a>>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ForEachStatement<'a> {
    pub initialiser: Box<Expression<'a>>,
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct LocalForEachStatement<'a> {
    pub declaration: VariableDeclaration<'a>,
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ContinueStatement<'a>(pub Option<&'a str>);

#[derive(Debug, PartialEq)]
pub struct BreakStatement<'a>(pub Option<&'a str>);

#[derive(Debug, PartialEq)]
pub struct ReturnStatement<'a>(pub Option<Box<Expression<'a>>>);

#[derive(Debug, PartialEq)]
pub struct WithStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct LabelledStatement<'a> {
    pub label: &'a str,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct SwitchStatement<'a> {
    pub expression: Box<Expression<'a>>,
    pub block: CaseBlock<'a>,
}

#[derive(Debug, PartialEq)]
pub struct CaseBlock<'a> {
    pub clauses: Option<CaseClauses<'a>>,
    pub default_clause: Option<DefaultClause<'a>>,
    pub more_clauses: Option<CaseClauses<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct CaseClauses<'a>(pub std::vec::Vec<CaseClause<'a>>);

#[derive(Debug, PartialEq)]
pub struct CaseClause<'a> {
    pub expression: Box<Expression<'a>>,
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct DefaultClause<'a> {
    pub statements: Option<StatementList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ThrowStatement<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct TryStatement<'a> {
    pub statement: Box<Statement<'a>>,
    pub catch: Option<Catch<'a>>,
    pub finally: Option<Finally<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Catch<'a> {
    pub name: &'a str,
    pub statement: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Finally<'a> {
    pub statement: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub struct DebuggerStatement;

pub mod parsing {

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

    use nom::IResult;
    use parser::helpers::parsing::*;
    use parser::js::statements::*;
    use parser::js::terminals::parsing::*;
    use parser::js::expressions::parsing::*;

    named!(statement_list<&str, StatementList>, conv!(StatementList(many1!(statement))));

    named!(pub statement<&str, Statement>, alt!(
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

    named!(pub block<&str, Block>, do_parse!(
        keyword!("{") >>
        statements: opt!(statement_list) >>
        keyword!("}") >>
        (Block {
            statements: statements
        })
    ));

    named!(pub variable_statement<&str, VariableStatement>, do_parse!(
        kind: variable_declaration_kind >>
        declarations: variable_declaration_list!(kind) >>
        automatic_semicolon >>
        (VariableStatement {
            declarations: declarations,
        })
    ));

    #[allow(needless_pass_by_value)]
    fn variable_declaration_list(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclarationList> {
        conv!(i, VariableDeclarationList(separated_nonempty_list!(
            keyword!(","),
            variable_declaration!(k.clone())
        )))
    }

    #[allow(needless_pass_by_value)]
    fn variable_declaration_list_not_in(i: &str, k: VariableDeclarationKind) -> IResult<&str, VariableDeclarationList> {
        conv!(i, VariableDeclarationList(separated_nonempty_list!(
            keyword!(","),
            variable_declaration_not_in!(k.clone())
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

    named!(pub empty_statement<&str, EmptyStatement>, do_parse!(
        keyword!(";") >>
        (EmptyStatement)
    ));

    named!(pub expression_statement<&str, ExpressionStatement>, do_parse!(
        expression: expression_list >>
        automatic_semicolon >>
        (ExpressionStatement(Box::new(expression)))
    ));

    named!(pub if_statement<&str, IfStatement>, do_parse!(
        keyword!("if") >>
        keyword!("(") >>
        expression: expression_list >>
        keyword!(")") >>
        ok: statement >>
        ko: opt!(do_parse!(
            complete!(keyword!("else")) >>
            statement: statement >>
            (Box::new(statement))
        )) >>
        (IfStatement {
            expression: Box::new(expression),
            ok: Box::new(ok),
            ko: ko,
        })
    ));

    named!(iteration_statement<&str, IterationStatement>, alt!(
        do_parse!(
            keyword!("do") >>
            statement: statement >>
            keyword!("while") >>
            keyword!("(") >>
            expression: expression_list >>
            keyword!(")") >>
            automatic_semicolon >>
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

    named!(initialiser<&str, Expression>, do_parse!(
        complete!(keyword!("=")) >>
        expression: assignment_expression >>
        (expression)
    ));

    named!(initialiser_not_in<&str, Expression>, do_parse!(
        complete!(keyword!("=")) >>
        expression: assignment_expression_not_in >>
        (expression)
    ));

    named!(continue_statement<&str, ContinueStatement>, do_parse!(
        keyword!("continue") >>
        label: opt!(complete!(do_parse!(
            not!(line_terminator) >>
            label: js_identifier >>
            (label)
        ))) >>
        automatic_semicolon >>
        (ContinueStatement(label))
    ));

    named!(break_statement<&str, BreakStatement>, do_parse!(
        keyword!("break") >>
        label: opt!(complete!(do_parse!(
            not!(line_terminator) >>
            label: js_identifier >>
            (label)
        ))) >>
        automatic_semicolon >>
        (BreakStatement(label))
    ));

    named!(return_statement<&str, ReturnStatement>, do_parse!(
        keyword!("return") >>
        expression: opt!(complete!(do_parse!(
            not!(line_terminator) >>
            expression: expression_list >>
            (expression)
        ))) >>
        automatic_semicolon >>
        (ReturnStatement(box_opt(expression)))
    ));

    named!(pub with_statement<&str, WithStatement>, do_parse!(
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

    named!(labelled_statement<&str, LabelledStatement>, do_parse!(
        label: js_identifier >>
        keyword!(":") >>
        statement: statement >>
        (LabelledStatement {
            label: label,
            statement: Box::new(statement),
        })
    ));

    named!(pub switch_statement<&str, SwitchStatement>, do_parse!(
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
        default_and_more_clauses: opt!(do_parse!(
            default_clause: default_clause >>
            more_clauses: opt!(case_clauses) >>
            (default_clause, more_clauses)
        )) >>
        keyword!("}") >>
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
        statements: opt!(complete!(statement_list)) >>
        (CaseClause {
            expression: Box::new(expression),
            statements: statements,
        })
    ));

    named!(default_clause<&str, DefaultClause>, do_parse!(
        keyword!("default") >>
        keyword!(":") >>
        statements: opt!(complete!(statement_list)) >>
        (DefaultClause {
            statements: statements,
        })
    ));

    named!(throw_statement<&str, ThrowStatement>, do_parse!(
        keyword!("throw") >>
        not!(line_terminator) >>
        expression: expression_list >>
        automatic_semicolon >>
        (ThrowStatement(Box::new(expression)))
    ));

    named!(pub try_statement<&str, TryStatement>, do_parse!(
        keyword!("try") >>
        statement: conv!(Statement::Block(block)) >>
        catch_finally: alt!(
            do_parse!(
                catch: catch >>
                finally: finally >>
                (Some(catch), Some(finally))
            )
            |
            do_parse!(
                catch: catch >>
                (Some(catch), None)
            )
            |
            do_parse!(
                finally: finally >>
                (None, Some(finally))
            )
        ) >>
        (TryStatement {
            statement: Box::new(statement),
            catch: catch_finally.0,
            finally: catch_finally.1,
        })
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
        complete!(keyword!("finally")) >>
        statement: block >>
        (Finally {
            statement: statement,
        })
    ));

    named!(debugger_statement<&str, DebuggerStatement>, do_parse!(
        keyword!("debugger") >>
        automatic_semicolon >>
        (DebuggerStatement)
    ));

    #[cfg(test)]
    mod tests {
        use nom::ErrorKind;
        use super::*;

        #[test]
        fn block() {
            assert!(super::block("").is_incomplete());

            assert_eq!(
                super::block(" {} "),
                IResult::Done(" ", Block {
                    statements: None,
                })
            );

            {
                let statements = ";;";

                let input = format!(" {{{}}} ", statements);

                assert_eq!(
                    super::block(&input),
                    IResult::Done(" ", Block {
                        statements: Some(super::statement_list(statements).unwrap().1),
                    })
                );
            }

            assert!(super::block(" { ").is_incomplete());
        }

        #[test]
        fn variable_statement() {
            assert!(super::variable_statement("").is_incomplete());

            {
                let kind = "var";
                let declarations = "test";

                let input = format!(" {} {} ", kind, declarations);

                assert_eq!(
                    super::variable_statement(&input),
                    IResult::Done("", VariableStatement {
                        declarations: super::variable_declaration_list(declarations, super::variable_declaration_kind(kind).unwrap().1).unwrap().1,
                    })
                );
            }

            assert!(super::variable_statement(" var ").is_incomplete());
        }

        #[test]
        fn variable_declaration() {
            assert!(super::variable_declaration("", VariableDeclarationKind::Var).is_incomplete());

            {
                let name = "test";

                let input = format!(" {} ", name);

                assert_eq!(
                    super::variable_declaration(&input, VariableDeclarationKind::Var),
                    IResult::Done(" ", VariableDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        expression: None,
                        kind: VariableDeclarationKind::Var,
                    })
                );
            }

            {
                let name = "test";
                let initialiser = "= true";

                let input = format!(" {} {} ", name, initialiser);

                assert_eq!(
                    super::variable_declaration(&input, VariableDeclarationKind::Var),
                    IResult::Done(" ", VariableDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        expression: Some(Box::new(super::initialiser(initialiser).unwrap().1)),
                        kind: VariableDeclarationKind::Var,
                    })
                );
            }
        }

        #[test]
        fn variable_declaration_not_in() {
            assert!(super::variable_declaration_not_in("", VariableDeclarationKind::Var).is_incomplete());

            {
                let name = "test";

                let input = format!(" {} ", name);

                assert_eq!(
                    super::variable_declaration_not_in(&input, VariableDeclarationKind::Var),
                    IResult::Done(" ", VariableDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        expression: None,
                        kind: VariableDeclarationKind::Var,
                    })
                );
            }

            {
                let name = "test";
                let initialiser_not_in = "= true";

                let input = format!(" {} {} ", name, initialiser_not_in);

                assert_eq!(
                    super::variable_declaration_not_in(&input, VariableDeclarationKind::Var),
                    IResult::Done(" ", VariableDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        expression: Some(Box::new(super::initialiser_not_in(initialiser_not_in).unwrap().1)),
                        kind: VariableDeclarationKind::Var,
                    })
                );
            }
        }

        #[test]
        fn if_statement() {
            assert!(super::if_statement("").is_incomplete());

            {
                let expression = "test";
                let ok = "{}";

                let input = format!(" if ({}) {} ", expression, ok);

                assert_eq!(
                    super::if_statement(&input),
                    IResult::Done(" ", IfStatement {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        ok: Box::new(super::statement(ok).unwrap().1),
                        ko: None,
                    })
                );
            }

            {
                let expression = "test";
                let ok = "{}";
                let ko = "{}";

                let input = format!(" if ({}) {} else {} ", expression, ok, ko);

                assert_eq!(
                    super::if_statement(&input),
                    IResult::Done(" ", IfStatement {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        ok: Box::new(super::statement(ok).unwrap().1),
                        ko: Some(Box::new(super::statement(ko).unwrap().1)),
                    })
                );
            }

            assert!(super::if_statement(" if ").is_incomplete());
            assert!(super::if_statement(" if ( ").is_incomplete());
            assert!(super::if_statement(" if (test) ").is_incomplete());
            assert!(super::if_statement(" if (test) ; else ").is_incomplete());

            assert_eq!(super::if_statement(" if {} "), IResult::Error(ErrorKind::Tag));
            assert_eq!(super::if_statement(" if (test) else {} "), IResult::Error(ErrorKind::Alt));
        }

        #[test]
        fn iteration_statement() {
            assert!(super::iteration_statement("").is_incomplete());

            // do while
            {
                let statement = "{}";
                let expression = "test";

                let input = format!(" do {} while ({}) ", statement, expression);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done("", IterationStatement::DoWhileStatement(DoWhileStatement {
                        statement: Box::new(super::statement(statement).unwrap().1),
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" do ").is_incomplete());
            assert!(super::iteration_statement(" do ; ").is_incomplete());
            assert!(super::iteration_statement(" do ; while ").is_incomplete());
            assert!(super::iteration_statement(" do ; while ( ").is_incomplete());

            // while
            {
                let expression = "test";
                let statement = "{}";

                let input = format!(" while ({}) {} ", expression, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::WhileStatement(WhileStatement {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" while ").is_incomplete());
            assert!(super::iteration_statement(" while ( ").is_incomplete());
            assert!(super::iteration_statement(" while (test) ").is_incomplete());

            assert_eq!(super::iteration_statement(" while {} "), IResult::Error(ErrorKind::Alt));

            // local for
            {
                let declarations = "test";
                let statement = "{}";

                let input = format!(" for (var {};;) {} ", declarations, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::LocalForStatement(LocalForStatement {
                        declarations: super::variable_declaration_list_not_in(declarations, VariableDeclarationKind::Var).unwrap().1,
                        condition: None,
                        expression: None,
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            {
                let declarations = "test";
                let condition = "test";
                let expression = "test";
                let statement = "{}";

                let input = format!(" for (var {};{};{}) {} ", declarations, condition, expression, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::LocalForStatement(LocalForStatement {
                        declarations: super::variable_declaration_list_not_in(declarations, VariableDeclarationKind::Var).unwrap().1,
                        condition: Some(Box::new(super::expression_list(condition).unwrap().1)),
                        expression: Some(Box::new(super::expression_list(expression).unwrap().1)),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" for ").is_incomplete());
            assert!(super::iteration_statement(" for ( ").is_incomplete());
            assert!(super::iteration_statement(" for (var ").is_incomplete());
            assert!(super::iteration_statement(" for (var test; ").is_incomplete());
            assert!(super::iteration_statement(" for (var test;; ").is_incomplete());
            assert!(super::iteration_statement(" for (var test;;) ").is_incomplete());

            assert_eq!(super::iteration_statement(" for {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var) {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var test) {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var test;) {} "), IResult::Error(ErrorKind::Alt));

            // for
            {
                let statement = "{}";

                let input = format!(" for (;;) {} ", statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::ForStatement(ForStatement {
                        initialiser: None,
                        condition: None,
                        expression: None,
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            {
                let initialiser = "test";
                let condition = "test";
                let expression = "test";
                let statement = "{}";

                let input = format!(" for ({};{};{}) {} ", initialiser, condition, expression, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::ForStatement(ForStatement {
                        initialiser: Some(Box::new(super::expression_list_not_in(initialiser).unwrap().1)),
                        condition: Some(Box::new(super::expression_list(condition).unwrap().1)),
                        expression: Some(Box::new(super::expression_list(expression).unwrap().1)),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" for ").is_incomplete());
            assert!(super::iteration_statement(" for ( ").is_incomplete());
            assert!(super::iteration_statement(" for (; ").is_incomplete());
            assert!(super::iteration_statement(" for (;; ").is_incomplete());
            assert!(super::iteration_statement(" for (;;) ").is_incomplete());

            assert_eq!(super::iteration_statement(" for {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for () {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (;) {} "), IResult::Error(ErrorKind::Alt));

            // local for each
            {
                let declaration = "test";
                let expression = "test";
                let statement = "{}";

                let input = format!(" for (var {} in {}) {} ", declaration, expression, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::LocalForEachStatement(LocalForEachStatement {
                        declaration: super::variable_declaration_not_in(declaration, VariableDeclarationKind::Var).unwrap().1,
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" for ").is_incomplete());
            assert!(super::iteration_statement(" for ( ").is_incomplete());
            assert!(super::iteration_statement(" for (var ").is_incomplete());
            assert!(super::iteration_statement(" for (var test in ").is_incomplete());
            assert!(super::iteration_statement(" for (var test in test ").is_incomplete());
            assert!(super::iteration_statement(" for (var test in test) ").is_incomplete());

            assert_eq!(super::iteration_statement(" for {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for () {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var) {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var test) {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (var test in) {} "), IResult::Error(ErrorKind::Alt));

            // for each
            {
                let initialiser = "test";
                let expression = "test";
                let statement = "{}";

                let input = format!(" for ({} in {}) {} ", initialiser, expression, statement);

                assert_eq!(
                    super::iteration_statement(&input),
                    IResult::Done(" ", IterationStatement::ForEachStatement(ForEachStatement {
                        initialiser: Box::new(super::left_hand_side_expression(initialiser).unwrap().1),
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    }))
                );
            }

            assert!(super::iteration_statement(" for ").is_incomplete());
            assert!(super::iteration_statement(" for ( ").is_incomplete());
            assert!(super::iteration_statement(" for (test ").is_incomplete());
            assert!(super::iteration_statement(" for (test in ").is_incomplete());
            assert!(super::iteration_statement(" for (test in test ").is_incomplete());
            assert!(super::iteration_statement(" for (test in test) ").is_incomplete());

            assert_eq!(super::iteration_statement(" for {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for () {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (test) {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::iteration_statement(" for (test in) {} "), IResult::Error(ErrorKind::Alt));
        }

        #[test]
        fn continue_statement() {
            assert!(super::continue_statement("").is_incomplete());

            assert_eq!(
                super::continue_statement(" continue "),
                IResult::Done("", ContinueStatement(None))
            );

            {
                let label = "test";

                let input = format!(" continue {} ", label);

                assert_eq!(
                    super::continue_statement(&input),
                    IResult::Done("", ContinueStatement(
                        Some(super::js_identifier(label).unwrap().1)
                    ))
                );
            }

            assert_eq!(
                super::continue_statement(" continue\ntest "),
                IResult::Done("\ntest ", ContinueStatement(None))
            );
        }

        #[test]
        fn break_statement() {
            assert!(super::break_statement("").is_incomplete());

            assert_eq!(
                super::break_statement(" break "),
                IResult::Done("", BreakStatement(None))
            );

            {
                let label = "test";

                let input = format!(" break {} ", label);

                assert_eq!(
                    super::break_statement(&input),
                    IResult::Done("", BreakStatement(
                        Some(super::js_identifier(label).unwrap().1)
                    ))
                );
            }

            assert_eq!(
                super::break_statement(" break\ntest "),
                IResult::Done("\ntest ", BreakStatement(None))
            );
        }

        #[test]
        fn return_statement() {
            assert!(super::return_statement("").is_incomplete());

            assert_eq!(
                super::return_statement(" return "),
                IResult::Done("", ReturnStatement(None))
            );

            {
                let expression = "test";

                let input = format!(" return {} ", expression);

                assert_eq!(
                    super::return_statement(&input),
                    IResult::Done("", ReturnStatement(
                        Some(Box::new(super::expression_list(expression).unwrap().1))
                    ))
                );
            }

            assert_eq!(
                super::return_statement(" return\ntest "),
                IResult::Done("\ntest ", ReturnStatement(None))
            );
        }

        #[test]
        fn with_statement() {
            assert!(super::with_statement("").is_incomplete());

            {
                let expression = "test";
                let statement = "{}";

                let input = format!(" with ({}) {} ", expression, statement);

                assert_eq!(
                    super::with_statement(&input),
                    IResult::Done(" ", WithStatement {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statement: Box::new(super::statement(statement).unwrap().1),
                    })
                );
            }

            assert!(super::with_statement(" with ").is_incomplete());
            assert!(super::with_statement(" with ( ").is_incomplete());
            assert!(super::with_statement(" with (test) ").is_incomplete());

            assert_eq!(super::with_statement(" with {} "), IResult::Error(ErrorKind::Tag));
        }

        #[test]
        fn labelled_statement() {
            assert!(super::labelled_statement("").is_incomplete());

            {
                let label = "test";
                let statement = "{}";

                let input = format!(" {}: {} ", label, statement);

                assert_eq!(
                    super::labelled_statement(&input),
                    IResult::Done(" ", LabelledStatement {
                        label: super::js_identifier(label).unwrap().1,
                        statement: Box::new(super::statement(statement).unwrap().1),
                    })
                );
            }

            assert!(super::labelled_statement(" test: ").is_incomplete());
        }

        #[test]
        fn switch_statement() {
            assert!(super::switch_statement("").is_incomplete());

            {
                let expression = "test";
                let block = "{}";

                let input = format!(" switch ({}) {} ", expression, block);

                assert_eq!(
                    super::switch_statement(&input),
                    IResult::Done(" ", SwitchStatement {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        block: super::case_block(block).unwrap().1,
                    })
                );
            }

            assert!(super::switch_statement(" switch ").is_incomplete());
            assert!(super::switch_statement(" switch ( ").is_incomplete());
            assert!(super::switch_statement(" switch (test) ").is_incomplete());

            assert_eq!(super::switch_statement(" switch {} "), IResult::Error(ErrorKind::Tag));
        }

        #[test]
        fn case_block() {
            assert!(super::case_block("").is_incomplete());

            assert_eq!(
                super::case_block(" {} "),
                IResult::Done(" ", CaseBlock {
                    clauses: None,
                    default_clause: None,
                    more_clauses: None,
                })
            );

            {
                let clauses = "case test:";
                let default_clause = "default:";
                let more_clauses = "case test:";

                let input = format!(" {{{}{}{}}} ", clauses, default_clause, more_clauses);

                assert_eq!(
                    super::case_block(&input),
                    IResult::Done(" ", CaseBlock {
                        clauses: Some(super::case_clauses(clauses).unwrap().1),
                        default_clause: Some(super::default_clause(default_clause).unwrap().1),
                        more_clauses: Some(super::case_clauses(more_clauses).unwrap().1),
                    })
                );
            }

            assert!(super::case_block(" { ").is_incomplete());
        }

        #[test]
        fn case_clause() {
            assert!(super::case_clause("").is_incomplete());

            {
                let expression = "test";

                let input = format!(" case {}: ", expression);

                assert_eq!(
                    super::case_clause(&input),
                    IResult::Done(" ", CaseClause {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statements: None,
                    })
                );
            }

            {
                let expression = "test";
                let statements = "{}";

                let input = format!(" case {}: {}", expression, statements);

                assert_eq!(
                    super::case_clause(&input),
                    IResult::Done("", CaseClause {
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                        statements: Some(super::statement_list(statements).unwrap().1),
                    })
                );
            }

            assert!(super::case_clause(" case ").is_incomplete());
        }

        #[test]
        fn default_clause() {
            assert!(super::default_clause("").is_incomplete());

            assert_eq!(
                super::default_clause(" default: "),
                IResult::Done(" ", DefaultClause {
                    statements: None,
                })
            );

            {
                let statements = "{}";

                let input = format!(" default: {}", statements);

                assert_eq!(
                    super::default_clause(&input),
                    IResult::Done("", DefaultClause {
                        statements: Some(super::statement_list(statements).unwrap().1),
                    })
                );
            }

            assert!(super::default_clause(" default ").is_incomplete());
        }

        #[test]
        fn throw_statement() {
            assert!(super::throw_statement("").is_incomplete());

            {
                let expression = "test";

                let input = format!(" throw {} ", expression);

                assert_eq!(
                    super::throw_statement(&input),
                    IResult::Done("", ThrowStatement(
                        Box::new(super::expression_list(expression).unwrap().1)
                    ))
                );
            }

            assert_eq!(
                super::throw_statement(" throw\ntest "),
                IResult::Error(ErrorKind::Not)
            );
        }

        #[test]
        #[allow(panic_params)]
        fn try_statement() {
            assert!(super::try_statement("").is_incomplete());

            {
                let block = "{}";
                let catch = "catch (test) {}";

                let input = format!(" try {} {} ", block, catch);

                assert_eq!(
                    super::try_statement(&input),
                    IResult::Done(" ", TryStatement {
                        statement: Box::new(Statement::Block(super::block(block).unwrap().1)),
                        catch: Some(super::catch(catch).unwrap().1),
                        finally: None,
                    })
                );
            }

            {
                let block = "{}";
                let finally = "finally {}";

                let input = format!(" try {} {} ", block, finally);

                assert_eq!(
                    super::try_statement(&input),
                    IResult::Done(" ", TryStatement {
                        statement: Box::new(Statement::Block(super::block(block).unwrap().1)),
                        catch: None,
                        finally: Some(super::finally(finally).unwrap().1),
                    })
                );
            }

            {
                let block = "{}";
                let catch = "catch (test) {}";
                let finally = "finally {}";

                let input = format!(" try {} {} {} ", block, catch, finally);

                assert_eq!(
                    super::try_statement(&input),
                    IResult::Done(" ", TryStatement {
                        statement: Box::new(Statement::Block(super::block(block).unwrap().1)),
                        catch: Some(super::catch(catch).unwrap().1),
                        finally: Some(super::finally(finally).unwrap().1),
                    })
                );
            }

            assert!(super::try_statement(" try ").is_incomplete());
            assert!(super::try_statement(" try {} ").is_incomplete());
        }
    }

}