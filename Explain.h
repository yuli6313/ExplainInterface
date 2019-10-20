/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Explain.h
 *
 * Provenance interface for Souffle; works for compiler and interpreter
 *
 ***********************************************************************/

#pragma once

#include "ExplainProvenanceImpl.h"

#include <csignal>
#include <iostream>
#include <regex>
#include <string>
#include <unistd.h>
#include <stdio.h>
#include <fstream>

#ifdef USE_NCURSES
#include <ncurses.h>
#endif

#include <sys/wait.h>
#include <sys/types.h>
#include <typeinfo>


#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#include <algorithm>
#endif

#ifndef IMP_QUERY
#define IMP_QUERY
#endif

#ifdef IMP_QUERY
#include "SymbolTable.h"
#include "RamTypes.h"
#include <string>
#include <set>
#include <typeinfo>
#include <stdio.h>
#endif

#include "SouffleInterface.h"
#include "WriteStreamCSV.h"

#define MAX_TREE_HEIGHT 500
#define MAX_TREE_WIDTH 500

#ifdef USE_READLINE
    /* auto completion commands */
static char *auto_completion_cmd[] = {"setdepth", "explain", "subproof", "explainnegation", "rule",
    					"measure", "output", "format", "q", "exit", "quit", NULL};
static char *auto_completion_format[] = {"json", "proof", NULL};
static char *auto_completion_less[] = {"less", NULL};


/*
 * -less, use less to print the proof tree
 * relation -> path | edge
 * S -> setdepth {Number expr}
 * S -> explain|explainnegation {relation}() (-less){0,1}
 * S -> rule relation {number}
 * S -> subproof relation({number}) (-less){0,1}
 * S -> q/exit/quit $
 * S -> format {json|tree}
 * S -> 
 */
/*
int rm_whitespace(char* buf) {
    int i = 0;
    while (1) {
        if (buf[i] != ' ' && buf[i] != 0) {
            i++;
        } else {
            break;
        }
    }
    
}

char *less_generator(const char *test, int state) {
    static int list_index, len;
    char *name;

    if (!state) {
        list_index = 0;
        len = strlen(text);
    }
    while ((name = auto_completion_less[list_index++])) {
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

char *format_generator(const char *test, int state) {

    static int list_index, len;
    char *name;

    if (!state) {
        list_index = 0;
        len = strlen(text);
    }
    while ((name = auto_completion_format[list_index++])) {
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}
*/
char *cmd_generator(const char *text, int state)
{
    static int list_index, len;
    char *name;

    if (!state) {
        list_index = 0;
        len = strlen(text);
    }
    while ((name = auto_completion_cmd[list_index++])) {
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

char **str_name_completion(const char *text, int start, int end)
{
    rl_attempted_completion_over = 1;
    // command is need autocomplete
    if (start == 0) {
        return rl_completion_matches(text, cmd_generator);
    }
    // flags or arguments need to auto complete
    return NULL;
}
#endif

namespace souffle {

/* Equivalence class for variables in query command */
class Equivalence {
public:
    ~Equivalence() = default;
    Equivalence(char t, std::string s, std::pair<size_t, size_t> idx) : type(t), symbol(s) {
        indices.push_back(idx);
    }
    
    Equivalence(const Equivalence& o) = default;
    Equivalence& operator=(const Equivalence& o) = default;
    void push_back(std::pair<size_t, size_t> idx) {
        indices.push_back(idx);
    }

    // verify if elements at the indices are equivalent in the given product
    bool verify(const std::vector<tuple>& product) const {
        for (size_t i = 1; i < indices.size(); ++i) {
            if (product[indices[i].first][indices[i].second] != product[indices[i-1].first][indices[i-1].second]) {
                return false;
            }
        }
        return true;
    }

    // get the index of first element in the indices array
    std::pair<size_t, size_t> getFirstIdx() {
        return indices[0];
    }

    std::vector<std::pair<size_t, size_t>> getIndices() {
        return indices;
    }

    char getType() {
        return type;
    }
    
    std::string getSymbol() {
        return symbol;
    }

private:
    char type;
    std::string symbol;
    std::vector<std::pair<size_t, size_t>> indices; 

};

/* const constraints for values in query command */
class ConstConstr {
public:
    ConstConstr() = default;
    ~ConstConstr() = default;
    void push_back(std::pair<std::pair<size_t, size_t>, RamDomain> constr) {
        constConstrs.push_back(constr);
    }
    bool verify(const std::vector<tuple>& product) const {
        for (auto constr : constConstrs) {
            if (product[constr.first.first][constr.first.second] != constr.second) {
                return false;
            }
        }
        return true;
    }
    std::vector<std::pair<std::pair<size_t, size_t>, RamDomain>>& getConstrs() {
        return constConstrs;
    }

private:
    std::vector<std::pair<std::pair<size_t, size_t>, RamDomain>> constConstrs;
};

class ExplainConfig {
public:
    /* Deleted copy constructor */
    ExplainConfig(const ExplainConfig&) = delete;

    /* Deleted assignment operator */
    ExplainConfig& operator=(const ExplainConfig&) = delete;

    /* Obtain the global ExplainConfiguration */
    static ExplainConfig& getExplainConfig() {
        static ExplainConfig config;
        return config;
    }

    /* Configuration variables */
    std::unique_ptr<std::ostream> outputStream = nullptr;
    bool json = false;
    int depthLimit = 4;

private:
    ExplainConfig() = default;
};

class Explain {
public:
    ExplainProvenance& prov;

    Explain(ExplainProvenance& prov) : prov(prov) {
#ifdef USE_READLINE
	rl_attempted_completion_function = str_name_completion;
#endif
    }
    ~Explain() = default;

    /* Process a command, a return value of true indicates to continue, returning false indicates to break (if
      the command is q/exit) */
    bool processCommand(std::string& commandLine) {
        std::vector<std::string> command = split(commandLine, ' ', 1);
        if (command.empty()) {
            return true;
        }

        if (command[0] == "setdepth") {
            if (command.size() != 2) {
                printError("Usage: setdepth <depth>\n");
                return true;
            }
            try {
                ExplainConfig::getExplainConfig().depthLimit = std::stoi(command[1]);
            } catch (std::exception& e) {
                printError("<" + command[1] + "> is not a valid depth\n");
                return true;
            }
            printInfo("Depth is now " + std::to_string(ExplainConfig::getExplainConfig().depthLimit) + "\n");
        } else if (command[0] == "explain") {
            std::pair<std::string, std::vector<std::string>> query;
            if (command.size() != 2) {
                printError("Usage: explain relation_name(\"<string element1>\", <number element2>, ...)\n");
                return true;
            }
            query = parseTuple(command[1]);
            printTree(prov.explain(query.first, query.second, ExplainConfig::getExplainConfig().depthLimit));
        } else if (command[0] == "subproof") {
            std::pair<std::string, std::vector<std::string>> query;
            int label = -1;
            if (command.size() <= 1) {
                printError("Usage: subproof relation_name(<label>)\n");
                return true;
            }
            query = parseTuple(command[1]);
            label = std::stoi(query.second[0]);
            printTree(prov.explainSubproof(query.first, label, ExplainConfig::getExplainConfig().depthLimit));
        } else if (command[0] == "explainnegation") {
            std::pair<std::string, std::vector<std::string>> query;
            if (command.size() != 2) {
                printError(
                        "Usage: explainnegation relation_name(\"<string element1>\", <number element2>, "
                        "...)\n");
                return true;
            }
            query = parseTuple(command[1]);

            // a counter for the rule numbers
            size_t i = 1;
            std::string rules;

            // if there are no rules, then this must be an EDB relation
            if (prov.getRules(query.first).size() == 0) {
                printInfo("The tuple would be an input fact!\n");
                return true;
            }

            for (auto rule : prov.getRules(query.first)) {
                rules += std::to_string(i) + ": ";
                rules += rule;
                rules += "\n\n";
                i++;
            }
            printInfo(rules);

            printPrompt("Pick a rule number: ");

            std::string ruleNum = getInput();
            auto variables = prov.explainNegationGetVariables(query.first, query.second, std::stoi(ruleNum));

            // @ and @non_matching are special sentinel values returned by ExplainProvenance
            if (variables.size() == 1 && variables[0] == "@") {
                printInfo("The tuple exists, cannot explain negation of it!\n");
                return true;
            } else if (variables.size() == 1 && variables[0] == "@non_matching") {
                printInfo("The variable bindings don't match, cannot explain!\n");
                return true;
            } else if (variables.size() == 1 && variables[0] == "@fact") {
                printInfo("The rule is a fact!\n");
                return true;
            }

            std::map<std::string, std::string> varValues;
            for (auto var : variables) {
                printPrompt("Pick a value for " + var + ": ");
                varValues[var] = getInput();
            }

            printTree(prov.explainNegation(query.first, std::stoi(ruleNum), query.second, varValues));
        } else if (command[0] == "rule" && command.size() == 2) {
            auto query = split(command[1], ' ');
            if (query.size() != 2) {
                printError("Usage: rule <relation name> <rule number>\n");
                return true;
            }
            try {
                printInfo(prov.getRule(query[0], std::stoi(query[1])) + "\n");
            } catch (std::exception& e) {
                printError("Usage: rule <relation name> <rule number>\n");
            }
        } else if (command[0] == "measure") {
            try {
                printInfo(prov.measureRelation(command[1]));
            } catch (std::exception& e) {
                printError("Usage: measure <relation name>\n");
            }
        } else if (command[0] == "output") {
            if (command.size() == 2) {
                // assign a new filestream, the old one is deleted by unique_ptr
                ExplainConfig::getExplainConfig().outputStream = std::make_unique<std::ofstream>(command[1]);
            } else if (command.size() == 1) {
                ExplainConfig::getExplainConfig().outputStream = nullptr;
            } else {
                printError("Usage: output  [<filename>]\n");
            }
        } else if (command[0] == "format") {
            if (command.size() == 2 && command[1] == "json") {
                ExplainConfig::getExplainConfig().json = true;
            } else if (command.size() == 2 && command[1] == "proof") {
                ExplainConfig::getExplainConfig().json = false;
            } else {
                printError("Usage: format <json|proof>\n");
            }
        } else if (command[0] == "exit" || command[0] == "q" || command[0] == "quit") {
            // close file stream so that output is actually written to file
            printPrompt("Exiting explain\n");
            return false;
        } else if (command[0] == "query") {
            // if there is no given relations, return directly
            if (command.size() != 2) {
                printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                           "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
            }
            // vector rels stores command, args pair parsed by parseQueryTuple()
            std::vector<std::pair<std::string, std::vector<std::string>>> rels;
            std::regex varRegex("[a-zA-Z_][a-zA-Z_0-9]*", std::regex_constants::extended);
            std::regex symbolRegex("\"([^\"]*)\"", std::regex_constants::extended);
            std::regex numberRegex("[0-9]+", std::regex_constants::extended);
            std::regex relRegex(
                    "([a-zA-Z0-9_.-]*)[[:blank:]]*\\(([[:blank:]]*([0-9]+|\"[^\"]*\"|[a-zA-Z_][a-zA-Z_0-9]*)([[:blank:]]*,[[:blank:]]*(["
                    "0-"
                    "9]+|\"[^\"]*\"|[a-zA-Z_][a-zA-Z_0-9]*))*)?\\)",
                    std::regex_constants::extended);
            std::smatch relMatcher;
            std::smatch argsMatcher;
            std::string relStr = command[1];
            // split the relations in the relation product string
            while (std::regex_search(relStr, relMatcher, relRegex)) {
                rels.push_back(parseQueryTuple(relMatcher[0]));
                if (rels.back().first.size() == 0 || rels.back().second.size() == 0) {
                    printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                               "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                    return true;
                }
                relStr = relMatcher.suffix().str();
            }
            if (rels.size() == 0) {
                printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                           "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                return true;
            }
            // map for variable name and corresponding equivalence class
            std::map<std::string, Equivalence> varMap;
            // const constraints that solution tuple must satisfy
            ConstConstr cc;
            // qpRels stores relation of tuples that contains at least one variable
            std::vector<Relation*> qpRels;
            // counter for adding element to qpRels
            size_t idx = 0;
            for (size_t i = 0; i < rels.size(); ++i) {
                Relation* r = prov.getSouffleProgram().getRelation(rels[i].first);
                std::vector<RamDomain> constTuple;
                if (r == nullptr) {
                    printError("Relation <" + rels[i].first + "> does not exist\n");
                    printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                               "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                    return true;
                }
                if (r->getArity() - 2 != rels[i].second.size()) {
                    std::cout << "<" << rels[i].first << "> has arity of " << (r->getArity() - 2) << std::endl;
                    printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                               "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                    return true;
                }
                bool containVar = false;
                for (size_t j = 0; j < rels[i].second.size(); ++j) {
                    if (std::regex_match(rels[i].second[j], argsMatcher, varRegex)) {
                        containVar = true;
                        auto mapIt =  varMap.find(argsMatcher[0]);
                        if (mapIt == varMap.end()) {
                            varMap.insert({argsMatcher[0], Equivalence(*(r->getAttrType(j)), argsMatcher[0], std::make_pair(idx, j))});
                        } else {
                            mapIt->second.push_back(std::make_pair(idx, j));
                        }
                    } else if (std::regex_match(rels[i].second[j], argsMatcher, symbolRegex)) {
                        if (*(r->getAttrType(j)) != 's') {
                            std::cout << argsMatcher[0] << " is not a symbol" << std::endl;
                            printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                                           "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                            return true;
                        }
                        RamDomain rd = prov.getSouffleProgram().getSymbolTable().lookup(argsMatcher[1]);
                        cc.push_back(std::make_pair(std::make_pair(idx, j), rd));
                        if (!containVar) {
                            constTuple.push_back(rd);
                        }
                    } else if (std::regex_match(rels[i].second[j], argsMatcher, numberRegex)) {
                        if (*(r->getAttrType(j)) != 'i') {
                            std::cout << argsMatcher[0] << " is not a number" << std::endl;
                            printError("Usage: query relation_name_1(\"<string element1>\", <number element2>, ...), "
                                           "relation_name_2(\"<string element1>\", <number element2>, ...), ...\n");
                            return true;
                        }
                        RamDomain rd = std::stoi(argsMatcher[0]);
                        cc.push_back(std::make_pair(std::make_pair(idx, j), rd));
                        if (!containVar) {
                            constTuple.push_back(rd);
                        }
                    }
                }
                
                // if tuple does not contain any variable, check if relation contains this tuple
                if (!containVar) {
                    bool containTuple = false;
                    for (auto it = r->begin(); it != r->end(); ++it) {
                        bool eq = true;
                        for (size_t j = 0; j < constTuple.size(); ++j) {
                            if (constTuple[j] != (*it)[j]) {
                                eq = false;
                                break;
                            }
                        }
                        if (eq) {
                            containTuple = true;
                            break;
                        }
                    }
                    // if relation contains this tuple, remove all related constraints
                    if (containTuple) {
                        cc.getConstrs().erase(cc.getConstrs().end() - r->getArity() + 2, cc.getConstrs().end());
                        
                    } else { // otherwise, there will there is no solution for given query
                        std::cout << "No solution for given query" << std::endl;
                        return true;
                    }
                } else {
                    qpRels.push_back(r);
                    ++idx;
                }
            }
            
            std::vector<Relation::iterator> qpIt;
            std::vector<std::vector<RamDomain>> solutions;
            for (auto r : qpRels) {
                qpIt.push_back(r->begin());
            }
            size_t slot_width = 8;
            while (true) {
                bool isSolution = true;
                // the vector that contains the tuples the iterators currently points to
                std::vector<tuple> element;
                for (auto it : qpIt) {
                   element.push_back(*it);
                }
                // check if the tuples satisifies variable equivalence
                for (auto var : varMap) {
                    if (!var.second.verify(element)) {
                        isSolution = false;
                        break;
                    }
                }
                if (isSolution) {
                    // check if tuple satisifies const constraints
                    isSolution = cc.verify(element);
                }
                if (isSolution) {
                    std::vector<RamDomain> solution;
                    for (auto var : varMap) {
                        auto idx = var.second.getFirstIdx();
                        solution.push_back(element[idx.first][idx.second]);
                        // check the number of the solution
                        size_t solution_width;
                        if (var.second.getType() == 's') {
                            solution_width = prov.getSouffleProgram().getSymbolTable().resolve(element[idx.first][idx.second]).length();
                        } else {
                            solution_width = std::to_string(element[idx.first][idx.second]).length();
                        }
                        if (solution_width > slot_width) {
                            slot_width = solution_width;
                        }
                    }
                    solutions.push_back(solution);
                }
                // increment the iterators
                size_t i = qpRels.size() - 1;
                bool terminate = true;
                for (auto it = qpIt.rbegin(); it != qpIt.rend(); ++it) {
                    if ((++(*it)) != qpRels[i]->end()) {
                        terminate = false;
                        break;
                    } else {
                        (*it) = qpRels[i]->begin();
                        --i;
                    }
                }
                if (terminate) {
                    break;
                }
            }
            for (auto var : varMap) {
                if (var.first.length() > slot_width) {
                    slot_width = var.first.length();
                }
            }

            std::string line((slot_width + 1) * varMap.size() + 1, '-');
            std::cout << line << std::endl;
            std::cout << "|";
            for (auto var : varMap) {
                std::string s(slot_width, ' ');
                s.replace(0, var.first.length(), var.first);
                std::cout << s << "|";
            }
            std::cout << std::endl;
            std::cout << line << std::endl;
            for (size_t i = 0; i < solutions.size(); ++i) {
               size_t j = 0;
               std::cout << "|";
               for (auto var : varMap) {
                   std::string solu(slot_width, ' ');
                   if (var.second.getType() == 'i') {
                       solu.replace(0, std::to_string(solutions[i][j]).length(), std::to_string(solutions[i][j]));
                       std::cout << solu << "|";
                   } else {
                       solu.replace(0, prov.getSouffleProgram().getSymbolTable().resolve(solutions[i][j]).length(), prov.getSouffleProgram().getSymbolTable().resolve(solutions[i][j]));
                       std::cout << solu << "|";
                   }
                   ++j;
               }
               std::cout << std::endl;
               std::cout << line << std::endl;
            }
        } else {
            printError(
                    "\n----------\n"
                    "Commands:\n"
                    "----------\n"
                    "setdepth <depth>: Set a limit for printed derivation tree height\n"
                    "explain <relation>(<element1>, <element2>, ...): Prints derivation tree\n"
                    "explainnegation <relation>(<element1>, <element2>, ...): Enters an interactive\n"
                    "    interface where the non-existence of a tuple can be explained\n"
                    "subproof <relation>(<label>): Prints derivation tree for a subproof, label is\n"
                    "    generated if a derivation tree exceeds height limit\n"
                    "rule <relation name> <rule number>: Prints a rule\n"
                    "output <filename>: Write output into a file, or provide empty filename to\n"
                    "    disable output\n"
                    "format <json|proof>: switch format between json and proof-trees\n"
                    "exit: Exits this interface\n\n");
        }

        return true;
    }

    /* The main explain call */
    virtual void explain() = 0;

private:
    /* Get input */
    virtual std::string getInput() = 0;

    /* Print a command prompt, disabled for non-terminal outputs */
    virtual void printPrompt(const std::string& prompt) = 0;

    /* Print a tree */
    virtual void printTree(std::unique_ptr<TreeNode> tree) = 0;

    /* Print any other information, disabled for non-terminal outputs */
    virtual void printInfo(const std::string& info) = 0;

    /* Print an error, such as a wrong command */
    virtual void printError(const std::string& error) = 0;

    /**
     * Parse tuple, split into relation name and values
     * @param str The string to parse, should be something like "R(x1, x2, x3, ...)"
     */
    std::pair<std::string, std::vector<std::string>> parseTuple(const std::string& str) {
        std::string relName;
        std::vector<std::string> args;

        // regex for matching tuples
        // values matches numbers or strings enclosed in quotation marks
        std::regex relRegex(
                "([a-zA-Z0-9_.-]*)[[:blank:]]*\\(([[:blank:]]*([0-9]+|\"[^\"]*\")([[:blank:]]*,[[:blank:]]*(["
                "0-"
                "9]+|\"[^\"]*\"))*)?\\)",
                std::regex_constants::extended);
        std::smatch relMatch;

        // first check that format matches correctly
        // and extract relation name
        if (!std::regex_match(str, relMatch, relRegex) || relMatch.size() < 3) {
            return std::make_pair(relName, args);
        }

        // set relation name
        relName = relMatch[1];

        // extract each argument
        std::string argsList = relMatch[2];
        std::smatch argsMatcher;
        std::regex argRegex(R"([0-9]+|"[^"]*")", std::regex_constants::extended);

        while (std::regex_search(argsList, argsMatcher, argRegex)) {
            // match the start of the arguments
            std::string currentArg = argsMatcher[0];
            args.push_back(currentArg);

            // use the rest of the arguments
            argsList = argsMatcher.suffix().str();
        }

        return std::make_pair(relName, args);
    }

    /**
     * Parse tuple for query, split into relation name and values, including numbers, string and variables
     * @param str The string to parse, should be something like "R(x1, x2, x3, ...)"
     */
    std::pair<std::string, std::vector<std::string>> parseQueryTuple(const std::string& str) {
        std::string relName;
        std::vector<std::string> args;
        // regex for matching tuples
        // values matches numbers or strings enclosed in quotation marks
        std::regex relRegex(
                "([a-zA-Z0-9_.-]*)[[:blank:]]*\\(([[:blank:]]*([0-9]+|\"[^\"]*\"|[a-zA-Z_][a-zA-Z_0-9]*)([[:blank:]]*,[[:blank:]]*(["
                "0-"
                "9]+|\"[^\"]*\"|[a-zA-Z_][a-zA-Z_0-9]*))*)?\\)",
                std::regex_constants::extended);
        std::smatch relMatch;
        // first check that format matches correctly
        // and extract relation name
        if (!std::regex_match(str, relMatch, relRegex) || relMatch.size() < 3) {
            return std::make_pair(relName, args);
        }

        // set relation name
        relName = relMatch[1];

        // extract each argument
        std::string argsList = relMatch[2];
        std::smatch argsMatcher;
        std::regex argRegex(R"([0-9]+|"[^"]*"|[a-zA-Z_][a-zA-Z_0-9]*)", std::regex_constants::extended);
        while (std::regex_search(argsList, argsMatcher, argRegex)) {
            // match the start of the arguments
            std::string currentArg = argsMatcher[0];
            args.push_back(currentArg);

            // use the rest of the arguments
            argsList = argsMatcher.suffix().str();
        }

        return std::make_pair(relName, args);
    }
};

class ExplainConsole : public Explain {
public:
    ExplainConsole(ExplainProvenance& provenance) : Explain(provenance) {}

    /* The main explain call */
    void explain() override {
        printPrompt("Explain is invoked.\n");

        while (true) {
#ifndef USE_READLINE
            printPrompt("Enter command > ");
#endif
            std::string line = getInput();

            // a return value of false indicates that an exit/q command has been processed
            if (processCommand(line) == false) {
                break;
            }
        }
    }

private:

    /* Get input */
    std::string getInput() override {
        std::string line;
#ifndef USE_READLINE
        if (!getline(std::cin, line)) {
            // if EOF has been reached, quit
            line = "q";
        }
#else
	char *buf;
	if (!(buf = readline("Enter command > "))) {
	    line = "q";
	} else {
	    //rm_whitespace(buf);
	    add_history(buf);
	    line = buf;
	    free(buf);
	}
#endif
        return line;
    }

    /* Print a command prompt, disabled for non-terminal outputs */
    void printPrompt(const std::string& prompt) override {
        if (!isatty(fileno(stdin))) {
            return;
        }
        std::cout << prompt;
    }

    /* Print a tree */
    void printTree(std::unique_ptr<TreeNode> tree) override {
        if (!tree) {
            return;
        }

        // handle a file ostream output
        std::ostream* output;
        if (ExplainConfig::getExplainConfig().outputStream == nullptr) {
            output = &std::cout;
        } else {
            output = ExplainConfig::getExplainConfig().outputStream.get();
        }

        if (!ExplainConfig::getExplainConfig().json) {
            tree->place(0, 0);
            ScreenBuffer screenBuffer(tree->getWidth(), tree->getHeight());
            tree->render(screenBuffer);
#ifndef USE_LESS
            *output << screenBuffer.getString();
#else
            std::filebuf fb;
            fb.open("/tmp.txt", std::ios::out);
            std::ostream os(&fb);
            os << screenBuffer.getString();
            fb.close();

            pid_t pid = fork();

            if (pid == 0) {
            execl("/usr/bin/less","less" ,"/tmp.txt", NULL);
            } else if (pid > 0){
            waitpid(pid, NULL, 0);

            } else {
                    *output << "no less option available";
            }
            remove("/tmp.txt");
#endif

        } else {
            *output << "{ \"proof\":\n";
            tree->printJSON(*output, 1);
            *output << ",";
            prov.printRulesJSON(*output);
            *output << "}\n";
        }
    }
    /* Print any other information, disabled for non-terminal outputs */
    void printInfo(const std::string& info) override {
        if (!isatty(fileno(stdin))) {
            return;
        }
        std::cout << info;
    }

    /* Print an error, such as a wrong command */
    void printError(const std::string& error) override {
        std::cout << error;
    }
};

#ifdef USE_NCURSES
class ExplainNcurses : public Explain {
public:
    ExplainNcurses(ExplainProvenance& provenance) : Explain(provenance) {}

    /* The main explain call */
    void explain() override {
        if (ExplainConfig::getExplainConfig().outputStream == nullptr) {
            initialiseWindow();
            std::signal(SIGWINCH, nullptr);
        }

        printPrompt("Explain is invoked.\n");

        while (true) {
            clearDisplay();
            printPrompt("Enter command > ");
            std::string line = getInput();

            // a return value of false indicates that an exit/q command has been processed
            if (processCommand(line) == false) {
                break;
            }

            // refresh treePad and allow scrolling
            prefresh(treePad, 0, 0, 0, 0, maxy - 3, maxx - 1);
            scrollTree(maxx, maxy);
        }

        // clean up
        endwin();
    }

private:
    WINDOW* treePad = nullptr;
    WINDOW* queryWindow = nullptr;
    int maxx, maxy;

    /* Get input */
    std::string getInput() override {
        char buf[100];

        curs_set(1);
        echo();

        // get next command
        wgetnstr(queryWindow, buf, 100);
        noecho();
        curs_set(0);
        std::string line = buf;

        return line;
    }

    /* Print a command prompt, disabled for non-terminal outputs */
    void printPrompt(const std::string& prompt) override {
        if (!isatty(fileno(stdin))) {
            return;
        }
        werase(queryWindow);
        wrefresh(queryWindow);
        mvwprintw(queryWindow, 1, 0, prompt.c_str());
    }

    /* Print a tree */
    void printTree(std::unique_ptr<TreeNode> tree) override {
        if (!tree) {
            return;
        }

        if (!ExplainConfig::getExplainConfig().json) {
            tree->place(0, 0);
            ScreenBuffer screenBuffer(tree->getWidth(), tree->getHeight());
            tree->render(screenBuffer);
            wprintw(treePad, screenBuffer.getString().c_str());
        } else {
            if (ExplainConfig::getExplainConfig().outputStream == nullptr) {
                std::stringstream ss;
                ss << "{ \"proof\":\n";
                tree->printJSON(ss, 1);
                ss << ",";
                prov.printRulesJSON(ss);
                ss << "}\n";

                wprintw(treePad, ss.str().c_str());
            } else {
                std::ostream* output = ExplainConfig::getExplainConfig().outputStream.get();
                *output << "{ \"proof\":\n";
                tree->printJSON(*output, 1);
                *output << ",";
                prov.printRulesJSON(*output);
                *output << "}\n";
            }
        }
    }

    /* Print any other information, disabled for non-terminal outputs */
    void printInfo(const std::string& info) override {
        if (!isatty(fileno(stdin))) {
            return;
        }
        wprintw(treePad, info.c_str());
        prefresh(treePad, 0, 0, 0, 0, maxy - 3, maxx - 1);
    }

    /* Print an error, such as a wrong command */
    void printError(const std::string& error) override {
        wprintw(treePad, error.c_str());
        prefresh(treePad, 0, 0, 0, 0, maxy - 3, maxx - 1);
    }

    /* Initialise ncurses command prompt window */
    WINDOW* makeQueryWindow() {
        WINDOW* queryWindow = newwin(3, maxx, maxy - 2, 0);
        wrefresh(queryWindow);
        return queryWindow;
    }

    /* Initialise ncurses window */
    void initialiseWindow() {
        initscr();

        // get size of window
        getmaxyx(stdscr, maxy, maxx);

        // create windows for query and tree display
        queryWindow = makeQueryWindow();
        treePad = newpad(MAX_TREE_HEIGHT, MAX_TREE_WIDTH);

        keypad(treePad, true);
    }

    /* Allow scrolling of provenance tree */
    void scrollTree(int maxx, int maxy) {
        int x = 0;
        int y = 0;

        while (true) {
            int ch = wgetch(treePad);

            if (ch == KEY_LEFT) {
                if (x > 2) x -= 3;
            } else if (ch == KEY_RIGHT) {
                if (x < MAX_TREE_WIDTH - 3) x += 3;
            } else if (ch == KEY_UP) {
                if (y > 0) y -= 1;
            } else if (ch == KEY_DOWN) {
                if (y < MAX_TREE_HEIGHT - 1) y += 1;
            } else {
                ungetch(ch);
                break;
            }

            prefresh(treePad, y, x, 0, 0, maxy - 3, maxx - 1);
        }
    }

    /* Clear the tree display */
    void clearDisplay() {
        // reset tree display on each loop
        werase(treePad);
        prefresh(treePad, 0, 0, 0, 0, maxy - 3, maxx - 1);
    }
};
#endif

inline void explain(SouffleProgram& prog, bool ncurses = false) {
    ExplainProvenanceImpl prov(prog);

    if (ncurses) {
#ifdef USE_NCURSES
        ExplainNcurses exp(prov);
        exp.explain();
#else
        std::cout << "The ncurses-based interface is not enabled\n";
#endif
    } else {
        ExplainConsole exp(prov);
        exp.explain();
    }
}

// this is necessary because ncurses.h defines TRUE and FALSE macros, and they otherwise clash with our parser
#ifdef USE_NCURSES
#undef TRUE
#undef FALSE
#endif

}  // end of namespace souffle
