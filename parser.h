//----------------------------------------------------------------------
// NAME: Jason Lunder
// FILE: parser.h
// DATE: 2/27/21
// DESC: Parses through tokens provided by the lexer to check if they match the grammar rules of mypl
// 	 Builds up the AST
//----------------------------------------------------------------------

#ifndef PARSER_H
#define PARSER_H

#include "token.h"
#include "mypl_exception.h"
#include "ast.h"
#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <bits/stdc++.h>

class Parser
{
public:

  // create a new recursive descent parser
  Parser(const Lexer& program_lexer, const std::string& module_id, const std::string& working_directory, const std::string& file);

  // run the parser
  void parse(Program& program);
  
private:
  std::string working_directory;
  std::string module;
  std::string fname;
  Lexer lexer;
  Token curr_token;
  bool debug_flag = true;  
  // helper functions
  void debug(std::string msg);
  void advance();
  void eat(TokenType t, std::string err_msg);
  void error(std::string err_msg);
  bool is_operator(TokenType t);
  bool is_value(TokenType t);
  bool is_data_type(TokenType t);
  
  // recursive descent functions
  void tdecl(TypeDecl& t);
  void fdecl(FunDecl& f);
  void vdecls(std::list<VarDeclStmt*>& varDeclarationStmts, Token& module_id, std::string& file_path);
  void params(FunDecl& f);
  void stmts(std::list<Stmt*>& statements, Token& module_id, std::string& file_path);
  void stmt(std::list<Stmt*>& statements, Token& module_id, std::string& file_path);
  void vdecl_stmt(VarDeclStmt& vdeclStmt);
  void assign_stmt(AssignStmt& assignSt);
  void lvalue(AssignStmt& assignSt);
  void cond_stmt(IfStmt& ifs);
  void condt(IfStmt& ifs);
  void while_stmt(WhileStmt& whilestmt);
  void for_stmt(ForStmt& forSt);
  void import_stmt(ImportStmt& import_stmt);
  void call_expr(CallExpr& call_expr);
  void args(CallExpr& call_expr);
  void exit_stmt(ReturnStmt& exit_stmt);
  void expr(Expr& e);
  void rvalue(SimpleTerm& simple);
  void idrval(IDRValue& idr, Token tmp);
  // TODO: add additional recursive descent functions here
};


// constructor
Parser::Parser(const Lexer& program_lexer, const std::string& module_id, const std::string& directory, const std::string& f) : lexer(program_lexer), module(module_id), working_directory(directory), fname(f)
{
}

void Parser::debug(std::string msg)
{
  if(debug_flag)
    std::cout << "<" << msg << ">" << std::endl;
}

// Helper functions

void Parser::advance()
{
  curr_token = lexer.next_token();
}


void Parser::eat(TokenType t, std::string err_msg)
{
  if (curr_token.type() == t)
    advance();
  else
    error(err_msg);
}


void Parser::error(std::string err_msg)
{
  std::string s = err_msg + "found '" + curr_token.lexeme() + "'";
  int line = curr_token.line();
  int col = curr_token.column();
  std::string f = working_directory+fname;
  throw MyPLException(SYNTAX, s, line, col, f);
}


bool Parser::is_operator(TokenType t)
{
  return t == PLUS or t == MINUS or t == DIVIDE or t == MULTIPLY or
    t == MODULO or t == AND or t == OR or t == EQUAL or t == LESS or
    t == GREATER or t == LESS_EQUAL or t == GREATER_EQUAL or t == NOT_EQUAL;
}

/*
 * Returns true if the token type is a data type
 */
bool Parser::is_data_type(TokenType t)
{
  return t == INT_TYPE or t == DOUBLE_TYPE or t == STRING_TYPE or
    t == BOOL_TYPE or t == CHAR_TYPE;
}

/*
 * Returns true if the token type is a value type
 */
bool Parser::is_value(TokenType t)
{
  return t == INT_VAL or t == BOOL_VAL or t == CHAR_VAL or t == DOUBLE_VAL or t == STRING_VAL;
}


// Recursive-decent functions

/*
 * Starts the parsing of the input stream. Implements the <program> rule in the grammar of mypl
 */
void Parser::parse(Program& p)
{
  p.file_path = fname;
  std::cout<<curr_token.lexeme()<<std::endl;
  advance();
  std::cout<<curr_token.lexeme()<<std::endl;
  while (curr_token.type() != EOS) {
    std::cout<<"IN PARSE LOOP"<<curr_token.lexeme()<<std::endl;
    if(curr_token.type() == IMPORT or curr_token.type() == FROM)
    {
      ImportStmt* i = new ImportStmt();
      debug("enter Import stmt");
      import_stmt(*i);
      debug("exit Import stmt");
      for(Decl* d : i->decls)
        p.decls.push_back(d);
    }
    else if (curr_token.type() == TYPE)
    {
      TypeDecl* t = new TypeDecl();
      debug("enter TypeDecl");
      tdecl(*t);
      debug("exit TypeDecl");
      p.decls.push_back(t);
    }
    else
    {
      FunDecl* f = new FunDecl();
      debug("enter FDecl");
      fdecl(*f);
      debug("exit FDecl");
      p.decls.push_back(f);
    }
  }
  eat(EOS, "expecting end-of-file ");
}

void Parser::import_stmt(ImportStmt& node)
{
  debug("START IMPORT STMT");
  Token first = curr_token;
  node.from = first.type() == FROM;
  advance();
  if(curr_token.type() == ID)
    node.module_id = curr_token;
  else if(curr_token.type() == STRING_VAL)
    node.module_id = curr_token;
  else
    error("expecting module name, found "+curr_token.lexeme());
  node.originalID = curr_token;
  advance();
  if(curr_token.type() == DOT)
    error("Module names cannot include . \nfull filepaths must be written as a string");
  std::string module_name = node.module_id.lexeme();
  std::istream* input_stream;
  std::cout<<"input stream not yet created"<<std::endl;
  std::ifstream module_file;
  module_file.open(working_directory+"/"+module_name);
  std::string module_fname_actual = working_directory+"/"+module_name;
  if(module_file)
  {
    std::cout<<module_name<<"in first"<<std::endl;
    input_stream = new std::ifstream(working_directory+"/"+module_name);
    debug(module_name);
    module_name.erase(module_name.length()-5, module_name.length());
    debug(module_name);
  }
  else
  {
    std::cout<<working_directory+"/"+module_name<<".mypl"<<"in second"<<std::endl;
    std::ifstream module_file;
    module_file.open(working_directory+"/"+module_name+".mypl");
    module_fname_actual = working_directory+"/"+module_name+".mypl";
    std::cout<<module_file.is_open()<<std::endl;
    if(module_file)
      input_stream = new std::ifstream(working_directory+"/"+module_name+".mypl");
    else
      error("file not found: "+module_name+".mypl");
  }
  std::vector<std::string> module_path;
  std::stringstream check1(working_directory+"/"+module_name);
  std::string intermediate;
  while(getline(check1, intermediate, '/'))
    module_path.push_back(intermediate);
  std::string fname = module_path.back();
  module_path.pop_back();
  std::string internal_working_directory = "";
  for(int i = 0; i < module_path.size(); ++i)
  {
    if(i == module_path.size()-1)
      internal_working_directory += module_path[i];
    else
      internal_working_directory += module_path[i]+"/";
  }
  std::string next_working_directory = working_directory+"/"+internal_working_directory;
  std::cout<<"BEGIN LEXING"<<std::endl;
  Lexer lexer(*input_stream, module_fname_actual);
  std::cout<<"END LEXING"<<std::endl;
  Token tmp = curr_token;
  if(tmp.type() == AS)
    advance();
  std::cout<<next_working_directory<<std::endl;
  std::cout<<curr_token.lexeme()<<std::endl;
  node.as = tmp.type() == AS;
  Parser parser(lexer, (first.type() == FROM) ? "main" : (tmp.type() == AS) ? curr_token.lexeme() : module_name, next_working_directory, fname);
  Program* p = new Program();
  //std::cout<<"?????????"<<lexer.next_token().lexeme()<<std::endl;
  parser.parse(*p);
  if(first.type() == FROM)
  {
    eat(IMPORT, "expecting import after module reference with from");
    node.members.push_back(curr_token.lexeme());
    eat(ID, "Expecting Member name");
    while(curr_token.type() == COMMA)
    {
      advance();
      node.members.push_back(curr_token.lexeme());
      eat(ID, "Expecting Member name");
    }
    for(Decl* d : p->decls)
    {
      if(std::find(node.members.begin(), node.members.end(), d->id.lexeme()) != node.members.end())
	node.decls.push_back(d);
    }
    node.module_id = Token(ID, "main", 0, 0);
  }
  else
  {
    std::cout<<"MADE IT HERE"<<tmp.lexeme()<<std::endl;
    for(Decl*d : p->decls)
      node.decls.push_back(d);
    if(tmp.type() == AS)
    {
      node.module_id = curr_token;
      eat(ID, "Expected new name for module");
    }
    else
      node.module_id = Token(node.module_id.type(), module_name, node.module_id.line(), node.module_id.column());
  }
}

/*
 * implements the <tdecl> rule of the mypl grammar. Looks for and processes through first a type token, then an id, then the rule <vdecl>, then end.
 */
void Parser::tdecl(TypeDecl& node)
{
  node.module_id = Token(ID, module, 0, 0);
  node.file_path = fname;
  eat(TYPE, "Expecting type in type declaration ");
  node.id = curr_token;
  eat(ID, "Expecting identifier in type declaration ");
  debug("enter vDecls");
  vdecls(node.vdecls, node.module_id, node.file_path); 
  debug("exit vDecls");
  eat(END, "Expecting end in type declaration ");
}

/*
 * Implements the <vdecls> rule of the mypl grammar.
 * loops recursively to allow for 0 or more instances of <vdecl_stmt><vdecls>
 */
void Parser::vdecls(std::list<VarDeclStmt*>& varDeclarationStmts, Token& module_id, std::string& file_path)
{
  if(curr_token.type() == VAR)
  {
    VarDeclStmt* vdeclStmt = new VarDeclStmt();
    vdeclStmt->module_id = module_id;
    vdeclStmt->file_path = file_path;
    debug("enter VarDeclStmt");
    vdecl_stmt(*vdeclStmt);
    debug("exit VarDeclStmt");
    varDeclarationStmts.push_back(vdeclStmt);
    debug("enter vDecls");
    vdecls(varDeclarationStmts, module_id, file_path);
    debug("exit vDecls");
  }
}

/*
 * Implements the <fdecl> rule of the mypl grammar for allowing function declarations
 * expects a FUN token, followed by a datatype or nil, then an ID, (, <params>, ), <stmts>, END
 */
void Parser::fdecl(FunDecl& node)
{
  node.module_id = Token(ID, module, 0, 0);
  node.file_path = fname;
  eat(FUN, "Expecting fun in function declaration ");
  if(curr_token.type() == NIL)
  {
    node.return_type = curr_token;
    eat(NIL, "Expecting data type in function declaration ");
  }
  else if(is_data_type(curr_token.type()) or curr_token.type() == ID)
  {
    node.return_type = curr_token;
    advance();
  }
  else
    error("Expecting data type or identifier in function declaration ");
  node.id = curr_token;
  eat(ID, "Expecting identifier in function declaration");
  eat(LPAREN, "Expecting ( in function declaration ");
  debug("enter params");
  params(node);
  debug("exit params");
  eat(RPAREN, "Expecting ) in function declaration ");
  debug("enter stmts");
  stmts(node.stmts, node.module_id, node.file_path);
  debug("exit stmts");
  eat(END, "Expecting end in function definition ");
}

/*
 * Implements the <params> rule of the mypl grammar for allowing 0 more instances of a function parameter
 */
void Parser::params(FunDecl& node)
{
  if(curr_token.type() != RPAREN)
  {
    FunDecl::FunParam* fp = new FunDecl::FunParam();
    fp->id = curr_token;
    eat(ID, "Expecting identifier in parameter declaration ");
    eat(COLON, "Expecting : in parameter declaration ");
    if(curr_token.type() == ID or is_data_type(curr_token.type()))
    {
      fp->type = curr_token;
      advance();
    }
    else
      error("Expecting data type in parameter declaration, found "+curr_token.lexeme());
    node.params.push_back(*fp);
    while(curr_token.type() == COMMA)
    {
      fp = new FunDecl::FunParam(); 
      advance();
      fp->id = curr_token;
      eat(ID, "Expecting identifier in parameter declaration ");
      eat(COLON, "Expecting : in parameter declaration ");
      if(curr_token.type() == ID or is_data_type(curr_token.type()))
      {
        fp->type = curr_token;
        advance();
      }
      else
        error("Expecting data type in parameter declaration, found "+curr_token.lexeme());
      node.params.push_back(*fp);
    }
  }
}

/*
 * Implementst the <stmts> rule of the mypl grammar allowing for 0 or more statements to be included after a function declaration
 * These include <vdecl_stmt>, <assign_stmt>, <cond_stmt>, <while_stmt>, <for_stmt>, <call_expr> and <exit_stmt>
 */
void Parser::stmts(std::list<Stmt*>& statements, Token& module_id, std::string& file_path)
{
  if(curr_token.type() == VAR or curr_token.type() == ID or curr_token.type() == IF or curr_token.type() == WHILE or curr_token.type() == FOR or curr_token.type() == RETURN)
  {
    stmt(statements, module_id, file_path);
    debug("enter stmts");
    stmts(statements, module_id, file_path);
    debug("exit stmts");
  }
}

/*
 * Implements the <stmt> rule of the mypl grammar to differentiate between the various types of rules allowed in <stmts>. 
 * Requires 2 lookaheads to differenate between <assign_stmt> and <call_expr>, but only 1 for others.
 */
void Parser::stmt(std::list<Stmt*>& statements, Token& module_id, std::string& file_path)
{
  switch(curr_token.type())
  {
    case VAR:
    {
      VarDeclStmt* vds = new VarDeclStmt();
      vds->module_id = module_id;
      vds->file_path = file_path;
      debug("enter VarDeclStmt");
      vdecl_stmt(*vds);
      debug("exit VarDeclStmt");
      statements.push_back(vds);
      break;
    }
    case IF:
    {      
      IfStmt* ifs = new IfStmt();
      ifs->module_id = module_id;
      ifs->file_path = file_path;
      debug("enter IfStmt");
      cond_stmt(*ifs);
      debug("exit IfStmt");
      statements.push_back(ifs);
      break;
    }
    case WHILE:
    {      
      WhileStmt* whileStmt = new WhileStmt();
      whileStmt->module_id = module_id;
      whileStmt->file_path = file_path;
      debug("enter WhileStmt");
      while_stmt(*whileStmt);
      debug("exit WhileStmt");
      statements.push_back(whileStmt);
      break;
    }
    case FOR:
    {      
      ForStmt* forSt = new ForStmt();
      forSt->module_id = module_id;
      forSt->file_path = file_path;
      debug("enter ForStmt");
      for_stmt(*forSt);
      debug("exit ForStmt");
      statements.push_back(forSt);
      break;
    }
    case RETURN:
    {      
      ReturnStmt* returnSt = new ReturnStmt();
      returnSt->module_id = module_id;
      returnSt->file_path = file_path;
      debug("enter ReturnStmt");
      exit_stmt(*returnSt);
      debug("exit ReturnStmt");
      statements.push_back(returnSt);
      break;
    }
    case ID:
    {      
      Token tmp = curr_token;
      advance();
      switch(curr_token.type())
      {
        case DOT:
	case ASSIGN:
	{	  
	  AssignStmt* assignSt = new AssignStmt();
          assignSt->module_id = module_id;
          assignSt->file_path = file_path;
	  assignSt->lvalue_list.push_back(tmp);
      	  debug("enter AssignStmt");
	  assign_stmt(*assignSt);
      	  debug("exit AssignStmt");
	  statements.push_back(assignSt);
	  break;
	}
	case LPAREN:
 	{	  
	  CallExpr* cExpr = new CallExpr();
      	  cExpr->module_id = module_id;
          cExpr->file_path = file_path;
	  cExpr->function_id = tmp;
      	  debug("enter CallExpr");
	  call_expr(*cExpr);
      	  debug("exit CallExpr");
	  statements.push_back(cExpr);
	  break;
	}
	default:
	{
   	  error("Expecting . or = or ( in statement, found "+curr_token.lexeme());
	}
      }
      break;
    }
  }
}

/*
 * Implements the rule for <vdecl_stmt> in the mypl grammar
 * allows for a variable to be declared and assigned to a value.
 * Allows for a type to be declared or implied.
 */
void Parser::vdecl_stmt(VarDeclStmt& node)
{
  eat(VAR, "Expecting var in variable declaration ");
  debug(curr_token.lexeme());
  node.id = curr_token;
  eat(ID, "Expecting identifier in variable declaration ");
  if(curr_token.type() == COLON)
  {
    advance();
    if(curr_token.type() == ID or is_data_type(curr_token.type()))
    {
      Token* tmp = new Token(curr_token.type(), curr_token.lexeme(), curr_token.line(), curr_token.column());
      node.type = tmp;
      advance();
    }
    else
      error("Expecting data type in variable declaration, found "+curr_token.lexeme());
  }
  eat(ASSIGN, "Expecting = in variable declaration ");
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.expr = e;
}

/*
 * implements the rule for <assign_stmt> in the mypl grammar
 * allows for an assignment of an <lvalue> to an expression
 */
void Parser::assign_stmt(AssignStmt& node)
{
  debug("enter lvalue");
  lvalue(node);
  debug("enter lvalue");
  eat(ASSIGN, "Expecting = in assignment ");
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.expr = e;
}

/*
 * Implements <lvalue> rule in the mypl grammar
 * allows for an id with 0 or more dot references attached to it on the left side of an assignment
 */
void Parser::lvalue(AssignStmt& node)
{
  while(curr_token.type() == DOT)
  { 
    advance();
    node.lvalue_list.push_back(curr_token);
    eat(ID, "Expecting identifier in object reference member call ");
  }
}

/*
 * Implements the rule for <cond_stmt> in the mypl grammar
 * allows for if statements with an if followed by an expression then statements, closed with an end but not before being given the option to attache elseifs and else statements
 */
void Parser::cond_stmt(IfStmt& node) 
{
  eat(IF, "Expecting if in conditional statement ");
  node.if_part = new BasicIf();
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.if_part->expr = e;
  eat(THEN, "Expecting then in conditional statement ");
  debug("enter stmts");
  stmts(node.if_part->stmts, node.module_id, node.file_path);
  debug("exit stmts");
  debug("enter condt");
  condt(node);
  debug("exit condt");
  eat(END, "Expecting end in conditional statement ");
}

/*
 * Implements the rule for <condt> in the mypl grammar, allowing for continued elseif/else conditionals attached to the end of an if block.
 */
void Parser::condt(IfStmt& node)
{
  switch(curr_token.type())
  {
    case ELSEIF:
    {      
      advance();
      BasicIf* bif = new BasicIf();
      Expr* e = new Expr();
      e->module_id = node.module_id;
      e->file_path = node.file_path;
      bif->module_id = node.module_id;
      bif->file_path = node.file_path;
      debug("enter expr");
      expr(*e);
      debug("exit expr");
      bif->expr = e;
      eat(THEN, "Expecting then in conditional statement ");
      debug("enter stmts");
      stmts(bif->stmts, node.module_id, node.file_path);
      debug("exit stmts");
      node.else_ifs.push_back(bif);
      condt(node);
      break;
    }
    case ELSE:
    {      
      advance();
      debug("enter stmts");
      stmts(node.body_stmts, node.module_id, node.file_path);
      debug("exit stmts");
      break;
    }
  }
}

/*
 * Implements the rule for <while_stmt> in the mypl grammar for parsing while loop declarations and ends.
 * Requires the use of the correct conditional expressions in the declaration and allows any statements for the duration of the loop that follow the normal rules of the grammar
 */
void Parser::while_stmt(WhileStmt& node)
{
  eat(WHILE, "Expecting while in while statement ");
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.expr = e;
  eat(DO, "Expecting do in while statement ");
  debug("enter stmts");
  stmts(node.stmts, node.module_id, node.file_path);
  debug("exit stmts");
  eat(END, "Expecting end in while statement ");
}

/*
 * Implements the rule for <for_stmt> in the mypl grammar for parsing for loop declarations and ends. Requires the correct tokens and expressions within the declaration and allows any stmts within the loop
 */
void Parser::for_stmt(ForStmt& node)
{
  eat(FOR, "Expecting for in for statement ");
  node.var_id = curr_token;
  eat(ID, "Expecting identifier in for statement ");
  eat(ASSIGN, "Expecting = for statement ");
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.start = e;
  eat(TO, "Expecting to in for statement ");
  e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.end = e;
  eat(DO, "Expecting do in for statement ");
  debug("enter stmts");
  stmts(node.stmts, node.module_id, node.file_path);
  debug("exit stmts");
  eat(END, "Expecting end in for statement ");
}


/*
 * Implements the rule for <call_expr> in the mypl grammar for parsing function calls
 */
void Parser::call_expr(CallExpr& node)
{
  eat(LPAREN, "Expecting ( in call expression ");
  args(node);
  eat(RPAREN, "Expecting ) in call expression ");
}

/*
 * implements the rule for <args> in the mypl grammar for handling arguments passed into a function call
 */
void Parser::args(CallExpr& node)
{
  if(is_value(curr_token.type()) or curr_token.type() == ID or curr_token.type() == NEW or curr_token.type() == NEG)
  { 
    Expr* e = new Expr(); 
    e->module_id = node.module_id;
    e->file_path = node.file_path;
    debug("enter expr");
    expr(*e);
    debug("exit expr");
    node.arg_list.push_back(e);
    while(curr_token.type() == COMMA)
    {
      advance();
      Expr* e = new Expr(); 
      e->module_id = node.module_id;
      e->file_path = node.file_path;
      debug("enter expr");
      expr(*e);
      debug("exit expr");
      node.arg_list.push_back(e);
    }
  }
}


/*
 * implements the rule for <exit_stmt> in the mypl grammar for processing the return token and allowing an expression to be attached to it for returning a value
 */
void Parser::exit_stmt(ReturnStmt& node)
{
  eat(RETURN, "Expecting return in exit statement ");
  Expr* e = new Expr();
  e->module_id = node.module_id;
  e->file_path = node.file_path;
  debug("enter expr");
  expr(*e);
  debug("exit expr");
  node.expr = e;
}

/*
 * implements the rule for <expr> in the mypl grammar for handling various types of mathematical expressions
 */
void Parser::expr(Expr& node)
{
  switch(curr_token.type())
  {
    case NIL:
    case INT_VAL:
    case BOOL_VAL:
    case CHAR_VAL:
    case DOUBLE_VAL:
    case STRING_VAL:
    case NEW:
    case ID:
    case NEG:
    {    
      debug(curr_token.lexeme());   
      SimpleTerm* exprTerm = new SimpleTerm();
      exprTerm->module_id = node.module_id;
      exprTerm->file_path = node.file_path;
      debug("enter RValue");
      rvalue(*exprTerm);
      debug("exit RValue");
      node.first = exprTerm;
      break;
    }
    case NOT:
    {
      advance();
      node.negated = true;
      ComplexTerm *comp_term = new ComplexTerm;
      node.first = comp_term;

      Expr *comp_expr = new Expr;
      comp_expr->module_id = node.module_id;
      comp_expr->file_path = node.file_path;
      comp_term->expr = comp_expr;
      expr(*comp_expr);
      break;
    }
    case LPAREN:
    {      
      advance();
      ComplexTerm* exprTerm = new ComplexTerm();
      exprTerm->module_id = node.module_id;
      exprTerm->file_path = node.file_path;
      Expr* e = new Expr();
      e->module_id = node.module_id;
      e->file_path = node.file_path;
      debug("enter expr");
      expr(*e);
      debug("exit expr");
      exprTerm->expr = e;
      node.first = exprTerm;
      eat(RPAREN, "Expected ) in expression ");
      break;
    }
    default:
    {
      error("Could not resolve incomplete expression. Found "+curr_token.lexeme());
    }
  }
  if(is_operator(curr_token.type()))
  {
    Token* tmp = new Token(curr_token.type(), curr_token.lexeme(), curr_token.line(), curr_token.column());
    node.op = tmp;
    advance();
    Expr* e = new Expr();
    e->module_id = node.module_id;
    e->file_path = node.file_path;
    debug("enter expr");
    expr(*e);
    debug("exit expr");
    node.rest = e;
  }
}
/*
 * implements the rule for <rvalue> in mypl for defining which tokens are allowed as the \"right side\" values of an expression
 */
void Parser::rvalue(SimpleTerm& node) 
{
  switch(curr_token.type())
  {
    case NIL:
    case INT_VAL:
    case BOOL_VAL:
    case CHAR_VAL:
    case DOUBLE_VAL:
    case STRING_VAL:
    {      
      SimpleRValue* rValueTerm = new SimpleRValue();
      rValueTerm->module_id = node.module_id;
      rValueTerm->file_path = node.file_path;
      rValueTerm->value = curr_token;
      node.rvalue =  rValueTerm;
      advance();
      break;
    }
    case NEW:
    {      
      advance();
      NewRValue* rValueTerm = new NewRValue();
      rValueTerm->module_id = node.module_id;
      rValueTerm->file_path = node.file_path;
      rValueTerm->type_id = curr_token;
      node.rvalue =  rValueTerm;
      eat(ID, "Expected identifier in right hand side expression ");
      break;
    }
    case ID:
    {      
      Token tmp = curr_token;
      debug(curr_token.to_string());
      advance();
      debug(tmp.to_string());
      debug(curr_token.to_string());
      switch(curr_token.type())
      {
        
	case DOT:
	{	  
	  IDRValue* idrValue = new IDRValue();
          idrValue->module_id = node.module_id;
          idrValue->file_path = node.file_path;
	  idrValue->path.push_back(tmp);
	  debug("enter IDRValue"+tmp.lexeme());
	  idrval(*idrValue, tmp);
	  debug("exit IDRValue");
	  for(auto const& it : idrValue->path)
	    debug(it.to_string());
	  node.rvalue = idrValue;
	  break;
	}
	case LPAREN:
	{	  
	  CallExpr* cExpr = new CallExpr();
          cExpr->module_id = node.module_id;
          cExpr->file_path = node.file_path;
	  cExpr->function_id = tmp; 
      	  debug("enter CallExpr");
	  call_expr(*cExpr);
	  debug("exit CallExpr");
	  node.rvalue = cExpr;
	  break;
	}
	default:
	{
	  SimpleRValue* rval = new SimpleRValue;
          rval->module_id = node.module_id;
          rval->file_path = node.file_path;
	  rval->value = tmp;
	  node.rvalue = rval;
	}
      }
      break;
    }
    case NEG:
    {      
      advance();
      NegatedRValue* negatedR = new NegatedRValue();
      negatedR->module_id = node.module_id;
      negatedR->file_path = node.file_path;
      Expr* e = new Expr();
      e->module_id = node.module_id;
      e->file_path = node.file_path;
      debug("enter expr");
      expr(*e);
      debug("exit expr");
      negatedR->expr = e;
      node.rvalue = negatedR;
      break;
    }
    default:
    {
      error("expected rvalue ");
    }

  }
}

/*
 * Implements the rule for <idrval> in the mypl grammar, for allowing an id with 0 or more dot references attached to it on the right side of an expression
 */
void Parser::idrval(IDRValue& node, Token tmp)
{
  if(tmp.type() == DOT)
  {
    tmp = curr_token;
    eat(ID, "Expected identifier in object reference expression ");
    node.path.push_back(tmp);
  }
  while(curr_token.type() == DOT)
  {
    advance();
    node.path.push_back(curr_token);
    eat(ID, "Expected identifier in object reference expression ");
  }
}

// TODO: implement the recursive descent functions you declared above

#endif
