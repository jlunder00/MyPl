//----------------------------------------------------------------------
// NAME: S. Bowers
// FILE: mypl_exception.h
// DATE: Spring 2021
// DESC: Custom exception class for mypl errors. Breaks error messages
//       into phases of lexical analysis, syntax analysis, semantic
//       analysis and runtim.
//----------------------------------------------------------------------


#ifndef MYPL_EXCEPTION
#define MYPL_EXCEPTION


// the compilation stage where the error occurred
enum ExceptionType {LEXER, SYNTAX, SEMANTIC, RUNTIME};


// specialized exception for mypl implementation
class MyPLException : public std::exception
{
 public:

  // construct a "normal" error exception
  MyPLException(ExceptionType type, const std::string& msg, int line, int column, const std::string& file);

  // construct an error exception without a line and column
  MyPLException(ExceptionType type, const std::string& msg, const std::string& f);
  
  // return a string representation for printing
  std::string to_string() const;
  
 private:

  ExceptionType type;
  std::string message;
  bool has_line_column;
  int line;
  int column;
  std::string filename;

};


MyPLException::MyPLException(ExceptionType t, const std::string& m, int l, int c, const std::string& f)
  : type(t), message(m), line(l), column(c), has_line_column(true), filename(f)
{
}


MyPLException::MyPLException(ExceptionType t, const std::string& m, const std::string& f)
  : type(t), message(m), has_line_column(false), filename(f)
{
}


std::string MyPLException::to_string() const
{
  std::string s = "Lexer";
  switch(type) {
    case SYNTAX: s = "Parser"; break;
    case SEMANTIC: s = "Type"; break;
    case RUNTIME: s = "Runtime"; break;
  }
  s += " Error: " + message;
  if (has_line_column)
    s += " at line " + std::to_string(line) +
      " column " + std::to_string(column) + 
      ", in " + filename;
  return s;
}


#endif
