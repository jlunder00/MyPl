
project(MyPL)

cmake_minimum_required(VERSION 3.0)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "-O0")
set(CMAKE_BUILD_TYPE Debug)

# build executables
add_executable(mypl hw6.cpp)
add_executable(hw5 hw5.cpp)
#add_executable(hw4 hw4.cpp)
add_executable(hw3 hw3.cpp)
add_executable(hw2 hw2.cpp)
add_executable(hw1 hw1.cpp)
