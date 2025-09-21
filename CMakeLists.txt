cmake_minimum_required(VERSION 3.20)

project(pentacorn C)

set(CMAKE_C_FLAGS " -std=c23 ")

add_executable(pentacorn src/rv.c src/rv.h)