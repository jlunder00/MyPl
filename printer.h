//----------------------------------------------------------------------
// NAME: Jason Lunder
// FILE: parser.h
// DATE: 2/27/21
// DESC: Iterates through the AST built up by the parser to pretty print the code stored in it in a nicer format
//----------------------------------------------------------------------


#ifndef PRINTER_H
#define PRINTER_H

#include <iostream>
#include "ast.h"


class Printer : public Visitor
{
public:
  // constructor
  Printer(std::ostream& output_stream) : out(output_stream) {}

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
  std::ostream& out;
  int indent = 0;

  void inc_indent() {indent += 3;}
  void dec_indent() {indent -= 3;}
  std::string get_indent() {return std::string(indent, ' ');}

  void wrap_print(std::string);

};

/*
 * Pre: strToPrint contains the string to be printed to standard out
 * Post: The string is printed
 */
void Printer::wrap_print(std::string strToPrint)
{
  std::cout<<strToPrint;
}


// TODO: Implement the visitor functions 
// top-level
/*
 * Pre: Program node exists
 * Post: entire program has been pretty printed
 */
void Printer::visit(Program& node)
{
  for(auto const& item : node.decls)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }

}

/*
 * Pre: FunDecl node exists
 * Post: A function has been pretty printed
 */
void Printer::visit(FunDecl& node)
{
  wrap_print("fun "+node.return_type.lexeme()+" "+node.id.lexeme()+"(");
  if(node.params.size() > 0)
  {
    bool first = true;
    for(auto const& iterator : node.params)
    {
      if(first)
      {
        wrap_print(iterator.id.lexeme()+": "+iterator.type.lexeme());
	first = false;
      }
      else
        wrap_print(", "+iterator.id.lexeme()+": "+iterator.type.lexeme());
    }
  }
  wrap_print(")\n");
  inc_indent();
  for(auto const& item : node.stmts)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  dec_indent();
  wrap_print(get_indent()+"end\n");
}

void Printer::visit(ImportStmt& node)
{
  wrap_print((node.from) ? "from " : "import ");
  wrap_print(node.originalID.lexeme());
  if(node.from)
  {
    wrap_print("import ");
    for(int i = node.members.begin(); i < node.members.end(); ++i)
    {
      if(i+1 = node.members.end())
        break;
      wrap_print(i+", ");
    }
    wrap_print(node.members.back());
  }
  if(node.as)
    wrap_print("as "+node.module_id.lexeme());
}

/*
 * Pre: TypeDecl node exists
 * Post: A type declaration has been pretty printed (like a struct)
 */
void Printer::visit(TypeDecl& node)
{
  wrap_print("type "+node.id.lexeme()+"\n");
  inc_indent();
  for(auto const& item : node.vdecls)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  dec_indent();
  wrap_print(get_indent()+"end\n");
}

// statements
/*
 * Pre: VarDeclStmt node exists
 * Post: Variable Declaration has been pretty printed
 */
void Printer::visit(VarDeclStmt& node)
{
  wrap_print("var "+node.id.lexeme());
  if(node.type)
    wrap_print(": "+node.type->lexeme());
  wrap_print(" = ");
  if(node.expr)
    node.expr->accept(*this);
}

/*
 * Pre: AssignStmt node exists
 * Post: Assignment has been pretty printed
 */
void Printer::visit(AssignStmt& node)
{
  bool first = true;
  for(auto const& iterator : node.lvalue_list)
  {
    if(first)
    {
      wrap_print(iterator.lexeme());
      first = false;
    }
    else
      wrap_print("."+iterator.lexeme());
  }
  wrap_print(" = ");
  if(node.expr)
    node.expr->accept(*this);
}

/*
 * Pre: ReturnStmt node exists
 * Post: ReturnStmt has been pretty printed
 */
void Printer::visit(ReturnStmt& node)
{
  wrap_print("return ");
  if(node.expr)
    node.expr->accept(*this);
}

/*
 * Pre: IfStmt node exists
 * Post: IfStmt has been pretty printed
 */
void Printer::visit(IfStmt& node)
{
  wrap_print("if ");
  node.if_part->expr->accept(*this);
  wrap_print(" then\n");
  inc_indent();
  for(auto const& item : node.if_part->stmts)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  for(auto const& item : node.else_ifs)
  {
    dec_indent();
    wrap_print(get_indent()+"elseif ");
    item->expr->accept(*this);
    wrap_print(" then\n");
    inc_indent();
    for(auto const& i : item->stmts)
    {
      wrap_print(get_indent());
      i->accept(*this);
      wrap_print("\n");
    }
  }
  if(node.body_stmts.size() > 0)
  {
    dec_indent();
    wrap_print(get_indent()+"else\n");
    inc_indent();
  }
  for(auto const& item : node.body_stmts)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  dec_indent();
  wrap_print(get_indent()+"end"+get_indent());
}

/*
 * Pre: WhileStmt node exists
 * Post: WhileStmt has been pretty printed
 */
void Printer::visit(WhileStmt& node)
{
  wrap_print("while ");
  node.expr->accept(*this);
  wrap_print(" do\n");
  inc_indent();
  for(auto const& item : node.stmts)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  dec_indent();
  wrap_print(get_indent()+"end");
}

/*
 * Pre: ForStmt node exists
 * Post: For Statement has been pretty printed
 */
void Printer::visit(ForStmt& node)
{
  wrap_print("for "+node.var_id.lexeme()+" = ");
  node.start->accept(*this);
  wrap_print(" to ");
  node.end->accept(*this);
  wrap_print(" do\n");
  inc_indent();
  wrap_print(get_indent());
  for(auto const& item : node.stmts)
  {
    wrap_print(get_indent());
    item->accept(*this);
    wrap_print("\n");
  }
  dec_indent();
  wrap_print("end\n");
}

// expressions
/*
 * Pre: Expr node exists
 * Post: Expr has been pretty printed
 */
void Printer::visit(Expr& node)
{
  if(node.negated)
    wrap_print("not ");
  if(node.op)
  {
    wrap_print("(");
    node.first->accept(*this);
    wrap_print(" "+node.op->lexeme()+" ");
    node.rest->accept(*this);
    wrap_print(")");
  }
  else
    node.first->accept(*this);
}

/*
 * Pre: SimpleTerm node exists
 * Post: SimpleTerm has been pretty printed
 */
void Printer::visit(SimpleTerm& node)
{
  node.rvalue->accept(*this);
}

/*
 * Pre: ComplexTerm node exists
 * Post: ComplexTerm has been pretty printed
 */
void Printer::visit(ComplexTerm& node)
{
  node.expr->accept(*this);
}

// rvalues
/*
 * Pre: SimpleRValue node exists
 * Post: SimpleRValue has been pretty printed
 */
void Printer::visit(SimpleRValue& node)
{
  wrap_print(node.value.lexeme());
}

/*
 * Pre: NewRValue node exists
 * Post: NewRValue has been pretty printed
 */
void Printer::visit(NewRValue& node)
{
  wrap_print("new "+node.type_id.lexeme());
}

/*
 * Pre: CallExpr node exists
 * Post: CallExpr has been pretty printed
 */
void Printer::visit(CallExpr& node)
{
  wrap_print(node.function_id.lexeme()+"(");
  for(auto const& item : node.arg_list)
    item->accept(*this);
  wrap_print(")");
}

/*
 * Pre: IDRValue node exists
 * Post: IDRValue has been pretty printed
 */
void Printer::visit(IDRValue& node)
{
  bool first = true;
  for(auto const& iterator : node.path)
  {
    if(first)
    {
      wrap_print(iterator.lexeme());
      first = false;
    }
    else
      wrap_print("."+iterator.lexeme());
  }
}

/*
 * Pre: NegatedRValue node exists
 * Post: negatedRValue has been pretty printed
 */
void Printer::visit(NegatedRValue& node)
{
  wrap_print("neg");
  node.expr->accept(*this);
}


#endif
