# Purpose

This document introduces a Dyalog APL toolkit for constructing text SQL commands from individual data values, using proper SQL syntax, without the programmer needing to remember the exact syntax for each command term.  It provides a simplified method of constructing such commands that is much shorter and easier than building the statements with ordinary text strings and improves readability and reduces syntactical and typographical errors.

This toolkit and documentation is intended for use by reasonably experienced APL programmers that have a basic grasp of using a SQL database.  It is designed to make their planned use of SQL easier rather than to teach them how to use SQL or convince them to use SQL.  Fortunately, the programmer need not understand very much about the SQL syntax in order to use this toolkit.  Generally, the programmer should know what SQL commands like **Select** do and how to construct them out of the typical parts (such as **Select**, **From**, and **Where**).  This toolkit is modeled on the same syntax as SQL and just takes care of the detail formatting of the individual parts for most commands.

These tools do not actually interface with a SQL database themselves but simply construct the commands that are then passed to whatever SQL interface and database that has been chosen to use for the application.

# Introduction

SQL (herein pronounced “see-kwel”) databases are powerful tools for storing and processing most kinds of data and they work well when integrated into APL business applications.  APL has had its own data filing system that works fine for many programmers’ needs, but when systems become large or complex, a commercial database system will often provide much better capabilities for the programmer’s use.

However, the syntax of the SQL language is sometimes complex and verbose, particularly from the standpoint of an experienced APL programmer.  A toolkit would be of use to help the APL programmer with such complexities by assisting him in building SQL language commands in a flexible application environment.

Granted, there’s nothing in the SQL language that cannot be constructed in a straightforward manner by concatenation of text.  But SQL can be rather picky about formatting and grammar and it can be a chore to watch out for lots of little details, especially when it may need to be done hundreds to thousands of places in a large application.  In addition, it is sometimes necessary to write code that can run against multiple databases with slightly different command syntax, or the database system being used may have to change over time.  In order to avoid rewriting the code every time, a toolkit could make the necessary adjustments at run-time to operate with whatever the database system in use requires.

For instance, whenever a user can enter their own optional selection criteria as input, how many times should the expression `,(~0∊⍴where)/' Where ',where` need to be written to include optional restrictions? Wouldn’t it be nicer if there were a simple subroutine to do that kind of work automatically?  And while it’s at it, how about giving it 0 to n separate terms for that Where clause and have it string them together with the word And between each one?  Or even build each of those terms internally given a list of field names to compare with and the data (supplied by the application user, possibly with wildcard characters) to compare them to (each field optionally, of course, since not all such selection data will normally be entered every time)?  And don’t forget what happens to SQL commands when the user enters a Quote character in their data if there isn’t extra code included everywhere to double potential quotes.  (An unmatched quote is also a prime method that hackers use to break into web sites.)  Care must also be taken to handle any negative numbers that would require extra negative-sign formatting or large or fractional numbers that require a more-than-usual number of digits.  Well, those are some of the kinds of things a toolkit can do automatically.

Herein is provided just such a toolkit that can be included in an application (as a static namespace) and used to dynamically build SQL language commands on-the-fly, based on static, user-entered, or program-adjustable data.  It also makes short and simple commands even easier and more readable to include in the code.  So if an application is being built that needs more than a few simple, unconnected tables (which is why SQL is probably being used in the first place), here’s a way to quickly and easily write very readable code to produce executable SQL commands.

# Repository Organization

This is a member of the [APLTree project](https://github.com/aplteam/apltree) and is also available via the [Tatin package manager](https://github.com/aplteam/Tatin).

## The Distribution Directory

This directory contains a workspace copy of the code for those that desire that form.  However, it is expected that most distribution will be done with the individual source code text files in the Source directory. A namespace script is also available here for those that prefer that distribution mechanism.

## The Documentation Directory

This directory contains a PDF file with extensive documentation on the toolkit and its components.  It begins with an introduction to this toolkit and how its routines correspond to the common SQL clauses.  Following that is a detailed description of each one of the public routines, each with examples of use.  This is reference material on how to use each routine but it is described more conversationally than technically, or in the more complicated cases a tutorial-style discussion is used as an aid in comprehension.  A short section about toolkit internals follows, for those that would like to know.  The last chapter is a quick reference guide on the syntax and purpose of each of the public routines for easy reference when a reminder of usage is needed.

## The Source/SQLFns Directory

This directory is intended to be imported as a complete namespace and contains all the code needed to use this package.  It contains no external dependencies.  Most function names are identical to the SQL keywords which they support.  For instance, the `Select` function formats the SQL **Select** clause.  Additional routines are provided for composite commands or general utility use. A few other functions (with names beginning with "∆") are present in this namespace for internal use only rather than public use.

## The Source/SQLFns/API Directory

This directory contains only references to the actual SQLFns code.  It is here only to provide a limited API interface point for use by the [Tatin package manager](https://github.com/aplteam/Tatin). This namespace is not needed at all if the code is being imported without using Tatin.

## The Source/Testing Directory

This directory is its own namespace which contains facilities for testing all the SQLFns functions, which are expected to be found in the #.SQLFns namespace.  This code is provided only for testing the main toolkit and is not needed for any application use.

Most of the functions herein are named after the corresponding public routines in SQLFns.  Simply execute the desired function here to test the corresponding SQLFns routine.  If multiple functions are to be tested, the `Test` function may be invoked with a list of function names (in almost any reasonable structure and format) as a right argument.  These names may include an `*` wild-card character, so `Test '*'` will execute all the functions in the workspace.

For more details on using `Test` and the testing engine, see the [Tester package](https://github.com/DavinChurch/Tester) in the companion repository.