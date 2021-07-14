//----------------------------------------------------------------------
// NAME: Jason Lunder
// FILE: lexer.h
// DATE: 2/4/21
// DESC: A lexer for the mypl language. Processes an input stream into tokens that can be utilized
// by a parser to build the language. Calls to next_token build and return the next token in the stream
//----------------------------------------------------------------------

#ifndef LEXER_H
#define LEXER_H

#include <istream>
#include <string>
#include "token.h"
#include "mypl_exception.h"


class Lexer
{
public:

  // construct a new lexer from the input stream
  Lexer(std::istream& input_stream, std::string f);

  // return the next available token in the input stream (including
  // EOS if at the end of the stream)
  Token next_token();
  
private:

  // input stream, current line, and current column
  std::istream& input_stream;
  int line;
  int column;
  std::string& file;

  // return a single character from the input stream and advance
  char read();

  // return a single character from the input stream without advancing
  char peek();

  // create and throw a mypl_exception (exits the lexer)
  void error(const std::string& msg, int line, int column) const;
};


Lexer::Lexer(std::istream& input_stream, std::string f)
  : input_stream(input_stream), line(1), column(1), file(f)
{
}


char Lexer::read()
{
  return input_stream.get();
}


char Lexer::peek()
{
  return input_stream.peek();
}


void Lexer::error(const std::string& msg, int line, int column) const
{
  throw MyPLException(LEXER, msg, line, column, file);
}

/*Skip the character passed in, and keep track of the comment boolean to skip characters
 * that would otherwise be used if not in a comment block
 */
bool skipNextChar(char c, bool& comment)
{
  if(comment)
    return true;
  if(c == '#')
  {
    comment = true;
    return true;
  }
  return std::isspace(c);
}

/*Pre: An input stream exists
 * Post: the next token in mypl language in the input stream is generated, removed, and returned *
 */
Token Lexer::next_token()
{
  std::string simpleSymbols = "(),.:+-/%*";
  std::string involvedSymbolStarts = "=><!";
  std::string lexeme = "";
  char cur;
  bool comment = false;
  while(skipNextChar(peek(), comment))
  {
    cur = read();
    column++;
    if(cur == '\n')
    {
      line++;
      column = 1;
      comment = false;
    }
  }
  int startCol = column;
  int startLine = line;
  if(input_stream.eof())
    return *(new Token(EOS, "", startLine, startCol));
  else if(simpleSymbols.find(peek()) != std::string::npos)
  {
    cur = read();
    //if(peek() == '\n')
    //  ++line;
    lexeme += cur;
    switch(cur)
    {
      case '(':
	column++;
        return *(new Token(LPAREN, lexeme, startLine, startCol));
      case ')':
	column++;
        return *(new Token(RPAREN, lexeme, startLine, startCol));
      case '.':
	column++;
	if(isdigit(peek()))
	{
          lexeme += peek();
          error("expecting digit [0,9].[0,9], found ."+lexeme, startLine, startCol);
	}
	else
          return *(new Token(DOT, lexeme, startLine, startCol));
      case ',':
	column++;
        return *(new Token(COMMA, lexeme, startLine, startCol));
      case ':':
	column++;
        return *(new Token(COLON, lexeme, startLine, startCol));
      case '+':
	column++;
        return *(new Token(PLUS, lexeme, startLine, startCol));
      case '-':
	column++;
        return *(new Token(MINUS, lexeme, startLine, startCol));
      case '/':
	column++;
        return *(new Token(DIVIDE, lexeme, startLine, startCol));
      case '*':
	column++;
        return *(new Token(MULTIPLY, lexeme, startLine, startCol));
      case '%':
	column++;
        return *(new Token(MODULO, lexeme, startLine, startCol));
    }
  }
  else if(involvedSymbolStarts.find(peek()) != std::string::npos)
  {
    cur = read();
    lexeme += cur;
    switch(cur)
    {
      case '=':
	cur = peek();
        switch(cur)
	{
	  case '=':
	    column += 2;
	    return *(new Token(EQUAL, lexeme+read(), startLine, startCol));
	  default:
	    column++;
	    return *(new Token(ASSIGN, lexeme, startLine, startCol));
	}
      case '<': 
	cur = peek();
	switch(cur)
	{
	  case '=':
	    column += 2;
	    return *(new Token(LESS_EQUAL, lexeme+read(), startLine, startCol));
	  default:
	    column++;
	    return *(new Token(LESS, lexeme, startLine, startCol));
	}
      case '>': 
	cur = peek();
	switch(cur)
	{
	  case '=':
	    column += 2;
            return *(new Token(GREATER_EQUAL, lexeme+read(), startLine, startCol));
	  default:
	    column++;
	    return *(new Token(GREATER, lexeme, startLine, startCol));
    	}
      case '!':
	cur = peek();
	switch(cur)
	{
	  case '=':
            column += 2;
	    return *(new Token(NOT_EQUAL, lexeme+read(), startLine, startCol));
	  default:
	    error("expecting != found "+lexeme, startLine, startCol);
	}
    }
  }
  else if(peek() == '\'')
  {
    cur = read();
    int mark = column;
    ++column;
    if (peek() == '\\') {
      cur = read();
      lexeme += cur;
      cur = read();
      ++column;
      lexeme += cur;
    } 
    else if (peek() == '\'') {
      cur = read();
      ++column;
      error("invalid character '' ", line, column - 1);
    } 
    else {
      cur = read();
      ++column;
      lexeme += cur;
    }
    if (peek() == '\'') {
      cur = read();
      ++column;
      return Token(CHAR_VAL, lexeme, line, mark);
    } 
    // Otherwise we error
    else {
      error("missing apostrophe", line, column);
      exit(0);
    }
  } 
  else if(peek() == '\"')
  {
    int mark = column;
    cur = read();
    ++column;
    while(peek() != '\"' && peek() != EOF) {
      cur = read();
      ++column;
      if (cur == '\n' || cur == '\r') {
        error("expecting \" found end-of-line", line, column);
        exit(0);
      }
      lexeme += cur;
    }
    if (peek() == EOF) {
      error("expecting \" found end-of-file", line, column);
      exit(0);
    }
    if (peek() == '\"') {
      cur = read();
      ++column;
      return Token(STRING_VAL, lexeme, line, mark);
    }
    error("expecting closing '\"'", line, column);
    exit(0);
  }
  else if(isdigit(peek()))
  {
    while(isdigit(peek()))
    {
      cur = read();
      lexeme += cur;
      column++;
    }
    if(peek() == '.')
    {
      cur = read();
      lexeme += cur;
      if(!isdigit(peek()))
      	error("expecting digit [0-9] after .", startLine, startCol);
      while(isdigit(peek()))
      {
	cur = read();
	lexeme += cur;
        column++;
      }
      if(isalpha(peek()))
        error("Id names cannot start with digits.", startLine, startCol);
      else 
        return *(new Token(DOUBLE_VAL, lexeme, startLine, startCol));
    }
    else
    {
      if(isalpha(peek()))
        error("Id names cannot start with digits.", startLine, startCol);
      else 
      	return *(new Token(INT_VAL, lexeme, startLine, startCol));
    }
  }
  else if(isalpha(peek()))
  {
    while(isalpha(peek()) || isdigit(peek()) || peek() == '_' || peek() == '\\')
    {
      cur = read();
      lexeme = lexeme+cur;
      column++;
    }
    if(lexeme == "and")
      return *(new Token(AND, lexeme, startLine, startCol));
    else if(lexeme == "or")
      return *(new Token(OR, lexeme, startLine, startCol));
    else if(lexeme == "not")
      return *(new Token(NOT, lexeme, startLine, startCol));
    else if(lexeme == "type")
      return *(new Token(TYPE, lexeme, startLine, startCol));
    else if(lexeme == "while")
      return *(new Token(WHILE, lexeme, startLine, startCol));
    else if(lexeme == "for")
      return *(new Token(FOR, lexeme, startLine, startCol));
    else if(lexeme == "to")
      return *(new Token(TO, lexeme, startLine, startCol));
    else if(lexeme == "do")
      return *(new Token(DO, lexeme, startLine, startCol));
    else if(lexeme == "if")
      return *(new Token(IF, lexeme, startLine, startCol));
    else if(lexeme == "then")
      return *(new Token(THEN, lexeme, startLine, startCol));
    else if(lexeme == "elseif")
      return *(new Token(ELSEIF, lexeme, startLine, startCol));
    else if(lexeme == "else")
      return *(new Token(ELSE, lexeme, startLine, startCol));
    else if(lexeme == "end")
      return *(new Token(END, lexeme, startLine, startCol));
    else if(lexeme == "fun")
      return *(new Token(FUN, lexeme, startLine, startCol));
    else if(lexeme == "var")
      return *(new Token(VAR, lexeme, startLine, startCol));
    else if(lexeme == "return")
      return *(new Token(RETURN, lexeme, startLine, startCol));
    else if(lexeme == "new")
      return *(new Token(NEW, lexeme, startLine, startCol));
    else if(lexeme == "bool")
      return *(new Token(BOOL_TYPE, lexeme, startLine, startCol));
    else if(lexeme == "int")
      return *(new Token(INT_TYPE, lexeme, startLine, startCol));
    else if(lexeme == "double")
      return *(new Token(DOUBLE_TYPE, lexeme, startLine, startCol));
    else if(lexeme == "char")
      return *(new Token(CHAR_TYPE, lexeme, startLine, startCol));
    else if(lexeme == "string")
      return *(new Token(STRING_TYPE, lexeme, startLine, startCol));
    else if(lexeme == "true" || lexeme == "false")
      return *(new Token(BOOL_VAL, lexeme, startLine, startCol));
    else if(lexeme == "nil")
      return *(new Token(NIL, lexeme, startLine, startCol));
    else if(lexeme == "neg")
      return *(new Token(NEG, lexeme, startLine, startCol));
    else if(lexeme == "import")
      return *(new Token(IMPORT, lexeme, startLine, startCol));
    else if(lexeme == "from")
      return *(new Token(FROM, lexeme, startLine, startCol));
    else if(lexeme == "as")
      return *(new Token(AS, lexeme, startLine, startCol));
    else
      return *(new Token(ID, lexeme, startLine, startCol));
  }
  else
  {
    cur = read();
    lexeme += cur;
    error("Illegal symbol: "+lexeme, startLine, column);
  }
}


#endif
