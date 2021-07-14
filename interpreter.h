//----------------------------------------------------------------------
// NAME: Jason Lunder 
// FILE: interpreter.h
// DATE: 4/10/21
// DESC: interpreter for the mypl langauge
//----------------------------------------------------------------------


#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <iostream>
#include <unordered_map>
#include <regex>
#include "ast.h"
#include "symbol_table.h"
#include "data_object.h"
#include "heap.h"


class Interpreter : public Visitor
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

  // return code from calling main
  int return_code() const;

  
private:

  // return exception
  class MyPLReturnException : public std::exception {};
  
  // the symbol table 
  SymbolTable sym_table;

  // holds the previously computed value
  DataObject curr_val;

  // the heap
  Heap heap;

  // the next oid
  size_t next_oid = 0;
  
  // the functions (all within the global environment)
  std::unordered_map<std::string, std::list<std::pair<std::string, FunDecl*>>> functions;
  
  // the user-defined types (all within the global environment)
  std::unordered_map<std::string, std::list<std::pair<std::string, TypeDecl*>>> types;

  // the global environment id
  int global_env_id = 0;
  
  // the program return code
  int ret_code = 0;

  std::list<std::string> get_keys_fun(std::list<std::pair<std::string, FunDecl*>> list)
  {
    std::list<std::string> keys;
    for(auto const& p : list)
      keys.push_back(p.first);
  
  }

  std::list<std::string> get_keys_type(std::list<std::pair<std::string, TypeDecl*>> list)
  {
    std::list<std::string> keys;
    for(auto const& p : list)
      keys.push_back(p.first);
  }
  // error message
  void error(const std::string& msg, const Token& token, const std::string& f);
  void error(const std::string& msg, const std::string& f); 
};



int Interpreter::return_code() const
{
  return ret_code;
}

void Interpreter::error(const std::string& msg, const Token& token, const std::string& f)
{
  throw MyPLException(RUNTIME, msg, token.line(), token.column(), f);
}


void Interpreter::error(const std::string& msg, const std::string& f)
{
  throw MyPLException(RUNTIME, msg, f);
}


// TODO: finish the visitor functions

  // top-level
/*
 * Pre: Program has been type checked
 * Post: all statements in program have been interpreted
 */
void Interpreter::visit(Program& node)
{
  sym_table.push_environment();
  sym_table.add_namespace("main", node.file_path);
  global_env_id = sym_table.get_environment_id();
  for(Decl* d : node.decls)
    d->accept(*this);

  CallExpr expr;
  std::list<std::string> keys = get_keys_fun(functions["main"]);
  if(std::find(keys.begin(), keys.end(), "main") == keys.end())
    error("Main function not in main module", node.file_path);
  std::list<std::pair<std::string, FunDecl*>> pairs = functions["main"];
  for(auto const& it : pairs)
  {
    if(it.first == "main")
    {
      expr.function_id = it.second->id;
      break;
    }
  }
  expr.accept(*this);

  sym_table.pop_environment();
}

void Interpreter::visit(ImportStmt& node)
{
  return; 
}

/*
 * Pre: Function Declaration has been type checked and has not been added to the functions list
 * Post: function has been added to functions list
 */
void Interpreter::visit(FunDecl& node)
{
  if(!sym_table.name_exists(node.id.lexeme()))
    sym_table.add_name(node.id.lexeme());
  sym_table.push_environment();
  std::vector<std::string> info;
  if(functions.find(node.module_id.lexeme()) == functions.end())
  {
    std::pair<std::string, FunDecl*> p = {node.id.lexeme(), &node};
    std::list<std::pair<std::string, FunDecl*>> names {p};
    functions.insert({node.module_id.lexeme(), names});
  }
  else
    functions[node.module_id.lexeme()].push_back({node.id.lexeme(), &node});
  
  
  if(node.params.size() > 0)
  {
    for(auto const& iterator : node.params)
    {
      if(!sym_table.name_exists(iterator.id.lexeme()))
        sym_table.add_name(iterator.id.lexeme());
    }
  }
  sym_table.pop_environment();

}

/*
 * Pre: Type Declaration has been type checked and has not been added to the types list
 * Post: type has been added to types list
 */
void Interpreter::visit(TypeDecl& node)
{
  if(!sym_table.name_exists(node.id.lexeme()))
    sym_table.add_name(node.id.lexeme());
  sym_table.push_environment();
  for(auto const& item : node.vdecls)
    item->accept(*this);
  if(types.find(node.module_id.lexeme()) == types.end())
  {
    std::pair<std::string, TypeDecl*> p = {node.id.lexeme(), &node};
    std::list<std::pair<std::string, TypeDecl*>> names {p};
    types.insert({node.module_id.lexeme(), names});
  }
  else
    types[node.module_id.lexeme()].push_back({node.id.lexeme(), &node});
  sym_table.pop_environment();
}

  // statements
/*
 * Pre: Vardeclstmt has been type checked
 * Post: variable has been declared and added to symbol table, value added if one is assigned
 *
 */
void Interpreter::visit(VarDeclStmt& node)
{
  if(node.expr)
  {
    node.expr->accept(*this);
    if(curr_val.is_oid())
      ++next_oid;
  }
  else
    curr_val.set_nil();
  DataObject obj(curr_val);
  if(!sym_table.name_exists(node.id.lexeme()))
    sym_table.add_name(node.id.lexeme());
  sym_table.set_val_info(node.id.lexeme(), obj);
}

/*
 * Pre: AssignStmt has been type checked, id exists in symbol table
 * Post: value in symbol table/heap has been updated with rhs
 */
void Interpreter::visit(AssignStmt& node)
{
  if(node.expr)
    node.expr->accept(*this);
  if(curr_val.is_oid())
    next_oid++;
  
  if(node.lvalue_list.size() > 1)
  {
    if(!sym_table.has_val_info(node.lvalue_list.front().lexeme()))
      error("Variable accessed before initilization", node.lvalue_list.front(), node.file_path);
    DataObject lhs;
    sym_table.get_val_info(node.lvalue_list.front().lexeme(), lhs);
    size_t oid;
    lhs.value(oid);
    HeapObject obj;
    heap.get_obj(oid, obj);
    bool first = true;
    std::list<Token>::iterator iterator;
    int i = 0;
    for(iterator = node.lvalue_list.begin(); iterator != node.lvalue_list.end(); ++iterator)
    {
      if(first)
	first = false;
      else
      {
	obj.get_val(iterator->lexeme(), lhs);
	if(i < node.lvalue_list.size()-1)
	{
	  lhs.value(oid);
	  if(!heap.has_obj(oid))
	   error("Object accessed before initialization in heap", *(iterator), node.file_path);
	  heap.get_obj(oid, obj);
	}
	else
	{
	  obj.set_att(iterator->lexeme(), curr_val);
	  heap.set_obj(oid, obj);
	}
      }
      i++;
    }
  }
  else
  {
    if(!sym_table.name_exists(node.lvalue_list.front().lexeme()))
      sym_table.add_name(node.lvalue_list.front().lexeme());
    sym_table.set_val_info(node.lvalue_list.front().lexeme(), curr_val);
  }
}

/*
 * Pre: returnstmt has been type checked and exists
 * Post: the statements for the returnstmt have been interpreted
 */
void Interpreter::visit(ReturnStmt& node)
{
  node.expr->accept(*this);
  throw new MyPLReturnException;
}

/*
 * Pre: stmt has been type checked and exists
 * Post: the statements for the ifstmt have been interpreted
 */
void Interpreter::visit(IfStmt& node)
{
  sym_table.push_environment();
  node.if_part->expr->accept(*this);
  bool val;
  curr_val.value(val);
  if(val)
  {
    for(auto const& item : node.if_part->stmts)
      item->accept(*this);
    sym_table.pop_environment();
    return;
  }
  
  for(auto const& item : node.else_ifs)
  {
    item->expr->accept(*this);
    curr_val.value(val);
    if(val)
    {
      for(auto const& i : item->stmts)
        i->accept(*this);
      sym_table.pop_environment();
      return;
    }
  }
  if(node.body_stmts.size() > 0)
  {
    for(auto const& item : node.body_stmts)
      item->accept(*this);
  }
  sym_table.pop_environment();
}

/*
 * Pre: whilestmt has been type checked and exists
 * Post: the statements for the whilestmt have been interpreted
 */
void Interpreter::visit(WhileStmt& node)
{
  sym_table.push_environment();
  node.expr->accept(*this);
  bool val;
  curr_val.value(val);
  while(val)
  {
    for(auto const& item : node.stmts)
      item->accept(*this);
    node.expr->accept(*this);
    curr_val.value(val);
  }
  sym_table.pop_environment();
}

/*
 * Pre: ForStmt has been type checked and exists
 * Post: the statements for the forstmt have been interpreted
 */
void Interpreter::visit(ForStmt& node)
{
  // Create the for loop condition
  sym_table.push_environment();
  node.start->accept(*this);
  DataObject start(curr_val);
  if(!sym_table.name_exists(node.var_id.lexeme()))
    sym_table.add_name(node.var_id.lexeme());
  sym_table.set_val_info(node.var_id.lexeme(), start);

  node.end->accept(*this);
  DataObject end(curr_val);

  int s;
  start.value(s);
  int e;
  end.value(e);
  
  while(s <= e)
  {
    // Print all statements within the for body
    for (auto const &i : node.stmts)
      i->accept(*this);
    s++;
    start.set(s);
    sym_table.set_val_info(node.var_id.lexeme(), start);
  }

  sym_table.pop_environment();
}

  // expressions
/*
 * Pre: Expr has been type checked and exists
 * Post: the statements for the expr have been interpreted
 */
void Interpreter::visit(Expr& node)
{
  node.first->accept(*this);
  if(node.negated)
  {
    bool val;
    curr_val.value(val);
    curr_val.set(!val);
  }
  else if(node.op)
  {
    DataObject lhs_val = curr_val;
    node.rest->accept(*this);
    DataObject rhs_val = curr_val;
    TokenType op = node.op->type();
    if(op == MODULO)
    {
      int rhs;
      int lhs;
      lhs_val.value(lhs);
      rhs_val.value(rhs);
      curr_val.set(lhs%rhs);
    }
    else if(op == PLUS or op == MINUS or op == MULTIPLY or op == DIVIDE)
    {
      if (lhs_val.is_integer() or rhs_val.is_integer())
      {
        int l;
        lhs_val.value(l);
        int r;
        rhs_val.value(r);
	curr_val.set((op == PLUS) ? l+r : (op == MINUS) ? l-r : (op == MULTIPLY) ? l*r : l/r);
      }
      else if (lhs_val.is_double() or rhs_val.is_double())
      {
        double l;
        lhs_val.value(l);
        double r;
        rhs_val.value(r);
	curr_val.set((op == PLUS) ? l+r : (op == MINUS) ? l-r : (op == MULTIPLY) ? l*r : l/r);
      }
      else if ((lhs_val.is_char() or lhs_val.is_string()) and (rhs_val.is_char() or rhs_val.is_string()))
      {
        if (op == PLUS)
        {
          if (lhs_val.is_char() and rhs_val.is_char())
          {
            char l;
            lhs_val.value(l);
            char r;
            rhs_val.value(r);
            curr_val.set(l + r);
          }
          else if (lhs_val.is_char() and rhs_val.is_string())
          {
            char l;
            lhs_val.value(l);
            std::string r;
            rhs_val.value(r);
            curr_val.set(l + r);
          }
          else if (lhs_val.is_string() and rhs_val.is_char())
          {
            std::string l;
            lhs_val.value(l);
            char r;
            rhs_val.value(r);
            curr_val.set(l + r);
          }
          else
          {
            std::string l;
            lhs_val.value(l);
            std::string r;
            rhs_val.value(r);
            curr_val.set(l + r);
          }
        }
      }
    }
    else if (op == EQUAL or op == NOT_EQUAL)
    {
      // Check for all possible appearances of nil
      if ((lhs_val.is_nil() and !rhs_val.is_nil()) or (!lhs_val.is_nil() and rhs_val.is_nil()))
        curr_val.set((op == EQUAL) ? false : true);
      else if (lhs_val.is_nil() and rhs_val.is_nil())
        curr_val.set((op == EQUAL) ? true : false);
      else if (lhs_val.is_bool())
      {
        bool l;
        bool r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
      else if (lhs_val.is_char())
      {
        char l;
        char r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
      else if (lhs_val.is_double())
      {
        double l;
        double r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
      else if (lhs_val.is_integer())
      {
        int l;
        int r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
      else if (lhs_val.is_oid())
      {
        size_t l;
        size_t r;
        lhs_val.value(l);
        rhs_val.value(r);
	curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
      else if (lhs_val.is_string())
      {
        std::string l;
        std::string r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op == EQUAL) ? l==r : l!=r);
      }
    }
    else if (op == LESS or op == GREATER or op == LESS_EQUAL or op == GREATER_EQUAL)
    {
      if (lhs_val.is_integer())
      {
        int l;
        int r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op==LESS)?l<r:(op==GREATER)?l>r:(op==LESS_EQUAL)?(l<=r):(l>=r));
      }
      else if (lhs_val.is_double())
      {
        double l;
        double r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op==LESS)?l<r:(op==GREATER)?l>r:(op==LESS_EQUAL)?(l<=r):(l>=r));
      }
      else if (lhs_val.is_char())
      {
        char l;
        char r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op==LESS)?l<r:(op==GREATER)?l>r:(op==LESS_EQUAL)?(l<=r):(l>=r));
      }
      else if (lhs_val.is_string())
      {
        std::string l;
        std::string r;
        lhs_val.value(l);
        rhs_val.value(r);
        curr_val.set((op==LESS)?l<r:(op==GREATER)?l>r:(op==LESS_EQUAL)?(l<=r):(l>=r));
      }
    }
    else if(op == AND or op == OR)
    {
      bool lhs;
      bool rhs;
      lhs_val.value(lhs);
      rhs_val.value(rhs);
      curr_val.set((op == AND) ? lhs and rhs : lhs or rhs);
    }
  }
  
}

/*
 * Pre: SimpleTerm has been type checked and exists
 * Post: the statements for the SimpleTerm have been interpreted
 */
void Interpreter::visit(SimpleTerm& node)
{
  node.rvalue->accept(*this);
}

/*
 * Pre: complex term has been type checked and exists
 * Post: the statements for the complex term have been interpreted
 */
void Interpreter::visit(ComplexTerm& node)
{
  node.expr->accept(*this);
}

  // rvalues
/*
 * Pre: SimpleRvalue has been type checked and a simple r value exists
 * Post: the statements for the rvalue have been interpreted
 */
void Interpreter::visit(SimpleRValue& node)
{
  if(node.value.type() == CHAR_VAL)
    curr_val.set(node.value.lexeme().at(0));
  else if (node.value.type() == STRING_VAL)
    curr_val.set(node.value.lexeme());
  else if(node.value.type() == INT_VAL)
  {
    try
    {
      curr_val.set(std::stoi(node.value.lexeme()));
    }
    catch(const std::invalid_argument& e)
    {
      error("internal error", node.value, node.file_path);
    }
    catch(const std::out_of_range& e)
    {
      error("int out of range", node.value, node.file_path);
    }
  }
  else if(node.value.type() == DOUBLE_VAL)
  {
    try
    {
      curr_val.set(std::stod(node.value.lexeme()));
    }
    catch(const std::invalid_argument& e)
    {
      error("internal error", node.value, node.file_path);
    }
    catch(const std::out_of_range& e)
    {
      error("double out of range", node.value, node.file_path);
    }
  }
  else if(node.value.type() == BOOL_VAL)
  {
    if(node.value.lexeme() == "true")
      curr_val.set(true);
    else
      curr_val.set(false);
  }
  else if(node.value.type() == NIL)
    curr_val.set_nil();
  else
    sym_table.get_val_info(node.value.lexeme(), curr_val);
}

/*
 * Pre: NewRValue has been type checked and a new object value exists on the rhs
 * Post: the value has been associated with a name and oid and added to the heap
 */
void Interpreter::visit(NewRValue& node)
{
  HeapObject* obj = new HeapObject();

  TypeDecl* type;
  std::list<std::string> keys = get_keys_type(types[node.module_id.lexeme()]);
  if(std::find(keys.begin(), keys.end(), node.type_id.lexeme()) == keys.end())
    error("type not in module", node.file_path);
  std::list<std::pair<std::string, TypeDecl*>> pairs = types[node.module_id.lexeme()]; 
  for(auto const& it : pairs)
  {
    if(it.first == node.type_id.lexeme())
    { 
      type = it.second;
      break;
    }
  }

  //TypeDecl* type = types[node.type_id.lexeme()]; 
  for(VarDeclStmt* v : type->vdecls)
  {
    v->expr->accept(*this);
    obj->set_att(v->id.lexeme(), curr_val);
  }
  curr_val.set(next_oid);
  heap.set_obj(next_oid, *(obj));
}

/*
 * Pre: Call expr has been type checked and the function being called exists in the function list
 * Post: all statements in the called function have been interpreted
 */
void Interpreter::visit(CallExpr& node)
{
  std::string fun_name = node.function_id.lexeme();
  
  if(fun_name == "print")
  {
    node.arg_list.front()->accept(*this);
    std::string s = curr_val.to_string();
    s = std::regex_replace(s, std::regex("\\\\n"), "\n");
    s = std::regex_replace(s, std::regex("\\\\t"), "\t");
    std::cout<<s;
  }
  else if (fun_name == "stoi")
  {
    node.arg_list.front()->accept(*this);
    std::string s = curr_val.to_string();
    int num = std::stoi(s);
    curr_val.set(num);
  }
  else if (fun_name == "stod")
  {
    node.arg_list.front()->accept(*this);
    std::string s = curr_val.to_string();
    double num = std::stod(s);
    curr_val.set(num);
  }
  else if (fun_name == "itos")
  {
    node.arg_list.front()->accept(*this);
    int num;
    curr_val.value(num);
    std::string s = std::to_string(num);
    curr_val.set(s);
  }
  else if (fun_name == "dtos")
  {
    node.arg_list.front()->accept(*this);
    double num;
    curr_val.value(num);
    std::string s = std::to_string(num);
    curr_val.set(s);
  }
  else if (fun_name == "get")
  {
    node.arg_list.front()->accept(*this);
    int pos;
    curr_val.value(pos);
    
    node.arg_list.pop_front();
    node.arg_list.front()->accept(*this);
    std::string s = curr_val.to_string();

    char c = s.at(pos);
    curr_val.set(c);
  }
  else if (fun_name == "length")
  {
    node.arg_list.front()->accept(*this);
    std::string s = curr_val.to_string();

    int l = s.length();
    curr_val.set(l);
  }
  else if (fun_name == "read")
  {
    std::string in;
    std::cin >> in;
    curr_val.set(in);
  }

  else
  {
    FunDecl* fun_node;
    std::list<std::string> keys = get_keys_fun(functions[node.module_id.lexeme()]);
    if(std::find(keys.begin(), keys.end(), node.function_id.lexeme()) == keys.end())
      error("function not in module", node.file_path);
    std::list<std::pair<std::string, FunDecl*>> pairs = functions[node.module_id.lexeme()];
    for(auto const& it : pairs)
    {
      if(it.first == node.function_id.lexeme())
      { 
        fun_node = it.second;
        break;
      }
    }
    std::vector<DataObject> args; 
    for(Expr* e : node.arg_list)
    {
      e->accept(*this);
      DataObject newObj = curr_val;
      args.push_back(newObj);
    }
    int prev_env = sym_table.get_environment_id();
    sym_table.set_environment_id(global_env_id);
    sym_table.push_environment();
    StringVec arg_names;
    for(auto const& p : fun_node->params)
      arg_names.push_back(p.id.lexeme());
    
    for(int i = 0; i < arg_names.size(); ++i)
    {
      if(!sym_table.name_exists(arg_names.at(i)))
        sym_table.add_name(arg_names.at(i));
      sym_table.set_val_info(arg_names.at(i), args.at(i));
    }
    try
    {
      for(Stmt* s : fun_node->stmts)
	s->accept(*this);
    }
    catch(MyPLReturnException* e)
    {
      if(fun_node->id.lexeme() == "main")
      {
	int r;
	curr_val.value(r);
	ret_code = r;
	return_code();
      }
    }

    sym_table.pop_environment();
    sym_table.set_environment_id(prev_env);
  }
}

/*
 * Pre: IDRValue has been type checked, id value on rhs exists
 * Post: curr_val has been set to the value of the rhs id
 */
void Interpreter::visit(IDRValue& node)
{
  if(!sym_table.has_val_info(node.first_token().lexeme()))
    error("Variable accessed before initialized", node.first_token(), node.file_path);
  DataObject rhs = new DataObject();
  sym_table.get_val_info(node.first_token().lexeme(), rhs);
  if(node.path.size() > 1)
  {
    size_t oid;
    rhs.value(oid);
    HeapObject obj;
    heap.get_obj(oid, obj);
    bool first = true;
    std::list<Token>::iterator iterator;
    int i = 0;
    for(iterator = node.path.begin(); iterator != node.path.end(); ++iterator)
    {  
      if(first)
        first = false;
      else
      {
        obj.get_val(iterator->lexeme(), rhs);
        if(i < node.path.size()-1)
        {
          rhs.value(oid);
	  if(!heap.has_obj(oid))
	    error("Heap object accessed before initialized", *(iterator), node.file_path);
	  heap.get_obj(oid, obj);
        }
        else
  	  curr_val = rhs;
      }
      ++i;
    }
  }
  else
    curr_val = rhs;
}

/*
 * Pre: negated rvalue has been type checked, a negated rvalue exists
 * Post: a numerical value has been negated and curr val is set to it
 */
void Interpreter::visit(NegatedRValue& node)
{
  if(node.expr)
  {
    node.expr->accept(*this);
    double curr_double;
    int curr_int;
    if(curr_val.value(curr_double))
      curr_val.set(-curr_double);
    else if(curr_val.value(curr_int))
      curr_val.set(-curr_int);
  }
}

#endif
