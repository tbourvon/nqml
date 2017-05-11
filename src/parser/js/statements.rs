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
        label: opt!(do_parse!(
            not!(line_terminator) >>
            label: js_identifier >>
            (label)
        )) >>
        automatic_semicolon >>
        (ContinueStatement(label))
    ));

    named!(break_statement<&str, BreakStatement>, do_parse!(
        keyword!("break") >>
        label: opt!(do_parse!(
            not!(line_terminator) >>
            label: js_identifier >>
            (label)
        )) >>
        automatic_semicolon >>
        (BreakStatement(label))
    ));

    named!(return_statement<&str, ReturnStatement>, do_parse!(
        keyword!("return") >>
        expression: opt!(do_parse!(
            not!(line_terminator) >>
            expression: expression_list >>
            (expression)
        )) >>
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
        keyword!("}") >>
        default_and_more_clauses: opt!(do_parse!(
            default_clause: default_clause >>
            more_clauses: opt!(case_clauses) >>
            (default_clause, more_clauses)
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
        keyword!("finally") >>
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
    }

}