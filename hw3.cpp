//----------------------------------------------------------------------
// NAME: S. Bowers
// FILE: hw3.cpp
// DATE: Spring 2021
// DESC: HW-3 driver program for testing the parser.
//----------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include "token.h"
#include "mypl_exception.h"
#include "lexer.h"
#include "parser.h"


using namespace std;


int main(int argc, char* argv[])
{
  // use standard input if no input file given
  istream* input_stream = &cin;
  if (argc == 2)
    input_stream = new ifstream(argv[1]);

  std::vector<std::string> module_path;
  std::stringstream check1(argv[1]);
  std::string intermediate;
  while(getline(check1, intermediate, '/'))
    module_path.push_back(intermediate);
  std::string fname = module_path.back();
  module_path.pop_back();
  std::string internal_working_directory = "";
  for(int i = 0; i < module_path.size(); ++i)
    internal_working_directory += module_path[i];
  // create the lexer
  Lexer lexer(*input_stream, argv[1]);
  Parser parser(lexer, "main", internal_working_directory, fname);
  
  // read each token in the file until EOS or error
  try {
    Program ast_root_node;
    parser.parse(ast_root_node);
  } catch (MyPLException e) {
    cout << e.to_string() << endl;
    exit(1);
  }

}

