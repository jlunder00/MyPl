//----------------------------------------------------------------------
// NAME: 
// FILE: 
// DATE: 
// DESC:
//----------------------------------------------------------------------


#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

#include <iostream>
#include "ast.h"
#include "symbol_table.h"


class TypeChecker : public Visitor
{
public:

  // top-level
  void visit(Program& node);
  void visit(FunDecl& node);
  void visit(TypeDecl& node);
  // statements
  void visit(ImportStmt& node);
  void visit(VarDeclStmt& node);
  void visit(AssignStmt& node);
  void visit(ReturnStmt& node);
  void visit(IfStmt& node);
  void visit(WhileStmt& node);
  void visit(ForStmt& node);
  // expressions
  void visit(Expr& node);
  void visit(SimpleTerm& node);
  void visit(ComplexTerm& node);
  // rvalues
  void visit(SimpleRValue& node);
  void visit(NewRValue& node);
  void visit(CallExpr& node);
  void visit(IDRValue& node);
  void visit(NegatedRValue& node);

private:

  // the symbol table 
  SymbolTable sym_table;

  // the previously inferred type
  std::string curr_type;

  // helper to add built in functions
  void initialize_built_in_types();

  // error message
  void error(const std::string& msg, const Token& token, std::string& f);
  void error(const std::string& msg, std::string& f); 

};


void TypeChecker::error(const std::string& msg, const Token& token, std::string& f)
{
  throw MyPLException(SEMANTIC, msg, token.line(), token.column(), f);
}


void TypeChecker::error(const std::string& msg, std::string& f)
{
  throw MyPLException(SEMANTIC, msg, f);
}

/*
 * Pre: the symbol table exists
 * Post: built in functions and their argument/return types have been added to the symbol table at the global environment
 */
void TypeChecker::initialize_built_in_types()
{
  // print function
  sym_table.add_name("print");
  sym_table.set_vec_info("print", StringVec {"string", "nil","main"});
  // stoi function
  sym_table.add_name("stoi");
  sym_table.set_vec_info("stoi", StringVec {"string", "int","main"});  
  
  sym_table.add_name("stod");
  sym_table.set_vec_info("stod", StringVec {"string", "double","main"});  
  
  sym_table.add_name("itos");
  sym_table.set_vec_info("itos", StringVec {"int", "string","main"});  

  sym_table.add_name("dtos");
  sym_table.set_vec_info("dtos", StringVec {"double", "string","main"});  
  
  sym_table.add_name("get");
  sym_table.set_vec_info("get", StringVec {"int", "string", "char","main"});  
  
  sym_table.add_name("length");
  sym_table.set_vec_info("length", StringVec {"string", "int","main"});  
  
  sym_table.add_name("read");
  sym_table.set_vec_info("read", StringVec {"nil", "string","main"});  
  
  // TODO: finish the rest of the built-in functions: stod, itos,
  // dtos, get, length, and read
}


/*
 * Pre: Program node exists
 * Post: entire program has been type checked
 */
void TypeChecker::visit(Program& node)
{
  // push the global environment
  sym_table.push_environment();
  sym_table.add_namespace("main", node.file_path);
  curr_type = "";
  // add built-in functions
  initialize_built_in_types();
  // push 
  for (Decl* d : node.decls)
    d->accept(*this);
  // check for a main function
  if (sym_table.name_exists("main") and sym_table.has_vec_info("main")) {
    // TODO: finish checking that the main function is defined with
    // the correct signature
    StringVec main_boyos;
    sym_table.get_vec_info("main", main_boyos);
    if(main_boyos[main_boyos.size()-2] != "int" and main_boyos.size() > 1)
      error("incorrect main function declaration", node.file_path);
  }
  else {
    // NOTE: the only time the 1-argument version of error should be
    // called!
    error("undefined 'main' function", node.file_path);
  }
   // pop the global environment
  sym_table.pop_environment();
}

// TODO: Implement the remaining visitor functions

void TypeChecker::visit(ImportStmt& node)
{
  if(!sym_table.has_namespace(node.module_id.lexeme()))
    sym_table.add_namespace(node.module_id.lexeme(), node.file_path);
}

/*
 * Pre: FunDecl node exists
 * Post: A function has been type checked
 */
void TypeChecker::visit(FunDecl& node)
{
  sym_table.add_namespace(node.module_id.lexeme(), node.file_path);
  sym_table.add_name(node.id.lexeme());
  sym_table.push_environment();
  std::vector<std::string> info;
  
  if(node.params.size() > 0)
  {
    for(auto const& iterator : node.params)
    {  
      sym_table.add_name(iterator.id.lexeme());
      sym_table.set_str_info(iterator.id.lexeme(), iterator.type.lexeme());
      info.push_back(iterator.type.lexeme());
    }
  }
  info.push_back(node.return_type.lexeme());
  info.push_back(node.module_id.lexeme());
  sym_table.set_vec_info(node.id.lexeme(), info);
  
  sym_table.add_name("return");
  sym_table.set_str_info("return", node.return_type.lexeme());
  for(auto const& item : node.stmts)
    item->accept(*this);
  sym_table.pop_environment();
}

/*
 * Pre: TypeDecl node exists
 * Post: A type declaration has been type checked (like a struct)
 */
void TypeChecker::visit(TypeDecl& node)
{
  sym_table.add_namespace(node.module_id.lexeme(), node.file_path);
  sym_table.add_name(node.id.lexeme());
  sym_table.push_environment();
  StringMap map;
  for(auto const& item : node.vdecls)
  {
    item->accept(*this);
    map[item->id.lexeme()] = curr_type;
  }
  map["@module"] = node.module_id.lexeme(); // use @ at beginning to make it impossible for a variable to share the same name
  sym_table.pop_environment();
  sym_table.set_map_info(node.id.lexeme(), map);
}

// statements
/*
 * Pre: VarDeclStmt node exists
 * Post: Variable Declaration has been type checked
 */
void TypeChecker::visit(VarDeclStmt& node)
{
  if(sym_table.name_exists_in_curr_env(node.id.lexeme()))
    error("Variable "+node.id.lexeme()+" redefined in var declaration in module ", node.id, node.file_path);
  std::string rhs_type = "";
  if(node.expr)
  {
    node.expr->accept(*this);
    rhs_type = curr_type;
  }
  
  if(node.type)
  {
    if(rhs_type != node.type->lexeme() and rhs_type != "nil")
      error("invalid type for variable "+node.id.lexeme()+", in module ", node.id, node.file_path);
    else if(rhs_type == "nil")
      rhs_type = node.type->lexeme(); 
  }
  curr_type = rhs_type;
  sym_table.add_name(node.id.lexeme());
  sym_table.set_str_info(node.id.lexeme(), rhs_type);
}

/*
 * Pre: AssignStmt node exists
 * Post: Assignment has been type checked
 */
void TypeChecker::visit(AssignStmt& node)
{
  if(node.expr)
    node.expr->accept(*this);
  std::string rhs_type = curr_type;
  if(sym_table.has_namespace(node.lvalue_list.front().lexeme()))
    node.lvalue_list.pop_front();
  if(!sym_table.name_exists(node.lvalue_list.front().lexeme()))
  {
    std::cout<<sym_table.to_string()<<std::endl;
    error("Type Mismatch in module ", node.lvalue_list.front(), node.file_path);
  }
  std::string lhs_type = "";
  sym_table.get_str_info(node.lvalue_list.front().lexeme(), lhs_type);
  if(sym_table.has_map_info(lhs_type))
  {
    StringMap map;
    sym_table.get_map_info(lhs_type, map);
    bool first = true;
    for(auto const& iterator : node.lvalue_list)
    {
      if(first)
        first = false;
      else
      {
        if(!sym_table.has_map_info(lhs_type))
	  error("variable not a user defined type in module ", iterator, node.file_path);
	sym_table.get_map_info(lhs_type, map);
        if(map.find(iterator.lexeme()) == map.end())
          error("Value not in map", node.lvalue_list.front(), node.file_path);
	lhs_type = map[iterator.lexeme()];
      }
    }
  }
  else
    lhs_type = curr_type;
  
  if(lhs_type != rhs_type)
    error("Type Mismatch in module ", node.lvalue_list.front(), node.file_path);

}

/*
 * Pre: ReturnStmt node exists
 * Post: ReturnStmt has been type checked
 */
void TypeChecker::visit(ReturnStmt& node)
{
  std::string return_type = "";
  sym_table.get_str_info("return", return_type);
  if(node.expr)
  {
    node.expr->accept(*this);
    if(curr_type != return_type and curr_type != "nil")
      error("Invalid return type "+curr_type+". Expecting "+return_type+" or nil, in module ", node.expr->first->first_token(), node.file_path);
  }
}

/*
 * Pre: IfStmt node exists
 * Post: IfStmt has been type checked
 */
void TypeChecker::visit(IfStmt& node)
{
  sym_table.push_environment();
  node.if_part->expr->accept(*this);
  if(curr_type != "bool")
    error("Expecting bool expression in module ", node.if_part->expr->first_token(), node.file_path);
  for(auto const& item : node.if_part->stmts)
    item->accept(*this);
  sym_table.pop_environment();
  for(auto const& item : node.else_ifs)
  {
    sym_table.push_environment();
    item->expr->accept(*this);
    if(curr_type != "bool")
      error("Expecting bool expression in module ", item->expr->first_token(), node.file_path);
    for(auto const& i : item->stmts)
      i->accept(*this);
    sym_table.pop_environment();
  }
  if(node.body_stmts.size() > 0)
  {
    sym_table.push_environment();
    for(auto const& item : node.body_stmts)
      item->accept(*this);
    sym_table.pop_environment();
  }
}

/*
 * Pre: WhileStmt node exists
 * Post: WhileStmt has been type checked
 */
void TypeChecker::visit(WhileStmt& node)
{
  sym_table.push_environment();
  node.expr->accept(*this);
  if(curr_type != "bool")
    error("Expecting bool expression in module ", node.expr->first_token(), node.file_path);
  for(auto const& item : node.stmts)
    item->accept(*this);
  sym_table.pop_environment();
}

/*
 * Pre: ForStmt node exists
 * Post: For Statement has been type checked
 */
void TypeChecker::visit(ForStmt& node)
{
  sym_table.push_environment();
  node.start->accept(*this);
  if(curr_type != "int")
    error("Expecting int expression in module ", node.start->first_token(), node.file_path);
  std::string start_type = curr_type;
  node.end->accept(*this);
  if(curr_type != "int")
    error("Expecting int expression in module ", node.end->first_token(), node.file_path);
  std::string end_type = curr_type;
  if(start_type != "int" or end_type != "int")
    error("Invalid types, cannot perform for loop on non integer range in module ", node.var_id, node.file_path);
  sym_table.add_name(node.var_id.lexeme());
  sym_table.set_str_info(node.var_id.lexeme(), "int");
  for(auto const& item : node.stmts)
    item->accept(*this);
  sym_table.pop_environment();
}

  // expressions
/*
 * Pre: Expr node exists
 * Post: Expr has been type checked
 */
void TypeChecker::visit(Expr& node)
{
  if(node.op)
  {
    node.first->accept(*this);
    std::string lhs_type = curr_type;
    node.rest->accept(*this);
    std::string rhs_type = curr_type;
    std::string lex = node.op->lexeme();
    if(lex == "%")
    {
      if(lhs_type == "int" and rhs_type == "int")
        curr_type = "int";
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else if(lex == "+" or lex == "-" or lex == "*")
    {
      if(((lhs_type == "char" and (rhs_type == "char" or rhs_type == "string")) or (lhs_type == "string" and (rhs_type == "char" or rhs_type == "string"))) and lex == "+")
        curr_type = "string";
      else if(((lhs_type == "double" or rhs_type == "double") and (lhs_type == "int" or rhs_type == "int")) or (lhs_type == "double" and rhs_type == "double"))
	curr_type = "double";
      else if(lhs_type == "int" and rhs_type == "int")
	curr_type = "int";
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else if(lex == "/")
    {
      if((lhs_type == "int" or lhs_type == "double") and (rhs_type == "int" or rhs_type == "double"))
      {
	if(lhs_type == "double" or rhs_type == "double")
	  curr_type == "double";
	else
	  curr_type == "int";
      }
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else if(lex == "!=" or lex == "==")
    {
      if(lhs_type == rhs_type or (lhs_type == "nil" or rhs_type == "nil"))
	curr_type = "bool";
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else if(lex == ">=" or lex == "<=" or lex == ">" or lex == "<")
    {
      if((lhs_type == rhs_type) and (lhs_type == "int" or lhs_type == "double" or lhs_type == "string" or lhs_type == "char"))
	curr_type = "bool";
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else if(lex == "and" or lex == "or")
    {
      if(lhs_type == "bool" and rhs_type == "bool")
	curr_type = "bool";
      else
	error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
    }
    else
      error("Cannot preform operation "+node.op->lexeme()+" on types "+lhs_type+" and "+rhs_type+", in module ", node.first_token(), node.file_path);
  }    
  else
    node.first->accept(*this);
  if(node.negated && curr_type != "bool")
    error("Negating non-boolean value, in module ", node.first->first_token(), node.file_path);
}

/*
 * Pre: SimpleTerm node exists
 * Post: SimpleTerm has been type checked
 */
void TypeChecker::visit(SimpleTerm& node)
{
  node.rvalue->accept(*this);
}

/*
 * Pre: ComplexTerm node exists
 * Post: ComplexTerm has been type checked
 */
void TypeChecker::visit(ComplexTerm& node)
{
  node.expr->accept(*this);
}

  // rvalues
/*
 * Pre: SimpleRValue node exists
 * Post: SimpleRValue has been type checked
 */
void TypeChecker::visit(SimpleRValue& node)
{
  switch(node.value.type())
  {
    case STRING_VAL:
      curr_type = "string";
      break;
    case INT_VAL:
      curr_type = "int";
      break;
    case BOOL_VAL:
      curr_type = "bool";
      break;
    case DOUBLE_VAL:
      curr_type = "double";
      break;
    case CHAR_VAL:
      curr_type = "char";
      break;
    case NIL:
      curr_type = "nil";
      break;
    case ID:
      if(!sym_table.has_str_info(node.value.lexeme()))
	error("ID undefined, in module ", node.value, node.file_path);
      sym_table.get_str_info(node.value.lexeme(), curr_type);
      break;
    default:
      error("Invalid SimpleRValue, in module ", node.first_token(), node.file_path);
      break;
  }
}

/*
 * Pre: NewRValue node exists
 * Post: NewRValue has been type checked
 */
void TypeChecker::visit(NewRValue& node)
{
  curr_type = node.type_id.lexeme();
}

/*
 * Pre: CallExpr node exists
 * Post: CallExpr has been type checked
 */
void TypeChecker::visit(CallExpr& node)
{
  curr_type = "";
  if(!sym_table.name_exists(node.function_id.lexeme()))
    error("Reference does not exist in current environment, in module ", node.function_id, node.file_path);
  StringVec required_types;
  if(sym_table.has_vec_info(node.function_id.lexeme()))
    sym_table.get_vec_info(node.function_id.lexeme(), required_types);
  else
    error("Reference does not exist in current environment, in module ", node.function_id, node.file_path);
  StringVec given_types;
  for(auto const& item : node.arg_list)
  {
    item->accept(*this);
    given_types.push_back(curr_type);
  }
  if(given_types.size() > 0)
  {
    if(given_types.size()+1 != required_types.size())
    {
      for(auto const& i : given_types)
        std::cout<<i<<std::endl;
      for(auto const& i : required_types)
        std::cout<<i<<std::endl;
      error("No matching function call for given types, in module ", node.function_id, node.file_path); 
    }
    for(std::size_t i = 0; i < given_types.size(); i++)
    {
      if(given_types[i] != required_types[i] and given_types[i] != "nil")
        error("No matching function call for given types, in module ", node.function_id, node.file_path); 
    }
  }
  curr_type = required_types[required_types.size()-1];

}

/*
 * Pre: IDRValue node exists
 * Post: IDRValue has been type checked
 */
void TypeChecker::visit(IDRValue& node)
{
  if(sym_table.has_namespace(node.first_token().lexeme()))
    node.path.pop_front();
  if(!sym_table.name_exists(node.first_token().lexeme()))
    error("Type mismatch, in module ", node.first_token(), node.file_path);
  std::string rhs_type = "";
  sym_table.get_str_info(node.first_token().lexeme(), curr_type);
  if(!sym_table.has_map_info(curr_type))
    error(node.first_token().lexeme()+" does not exist within "+curr_type+", in module ", node.first_token(),  node.file_path);
  rhs_type = curr_type; 
  if(sym_table.has_map_info(rhs_type))
  {
    StringMap map;
    sym_table.get_map_info(curr_type, map);
    sym_table.get_map_info(rhs_type, map);
    bool first = true;
    for(auto const& iterator : node.path)
    {
      if(first)
        first = false;
      else
      {
        if(!sym_table.has_map_info(rhs_type))
	  error("variable not a user defined type, in module ", iterator, node.file_path);
        if(map.find(iterator.lexeme()) == map.end())
          error("Value not in map", node.first_token(), node.file_path);
        rhs_type = map[iterator.lexeme()];
        sym_table.get_map_info(rhs_type, map);
      }
    }
  }
  curr_type = rhs_type; 
}

/*
 * Pre: NegatedRValue node exists
 * Post: negatedRValue has been type checked
 */
void TypeChecker::visit(NegatedRValue& node)
{
  node.expr->accept(*this);
}


#endif
