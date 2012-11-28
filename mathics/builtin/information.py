# -*- coding: utf-8 -*-

"""
Information functions

Basic functions to get information about definitions in the current context.
"""


from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator
from mathics.core.expression import Expression, Symbol
from mathics.core.evaluation import AbortInterrupt, ReturnInterrupt, BreakInterrupt, ContinueInterrupt

from mathics.core.rules import BuiltinRule

from mathics.builtin.lists import _IterationFunction
from mathics.builtin.patterns import match,RuleDelayed 



def _get_usage_string(name,evaluation):
        ruleusage=evaluation.definitions.get_definition(name).get_values_list('messages')
        usagetext=None;
        for rulemsg in ruleusage:
            if  rulemsg.pattern.expr.leaves[1]=="usage":
                usagetext=rulemsg.replace.value
        return  usagetext            


class Definition(PrefixOperator):

    """
    Retrieves information about symbols
    """
    operator="?"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold')
    def apply(self, symbol, evaluation):
        'Definition[symbol_]'
        if type(symbol)!=Symbol: 
            print "Information::notfound:  Expression is not a symbol";
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None

        textusage=_get_usage_string(symbol.name,evaluation)
        if textusage!=None:
            print textusage
            print
            return Symbol('Null')
        
        from mathics.core.expression import from_python        
        print definition.context+ symbol.name+"\n"
        
        if definition.ownvalues !=None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        print evaluation.format_output(from_python(ownval.pattern.expr)),\
                     eqs,evaluation.format_output(from_python(ownval.replace))
                print ""


        if definition.upvalues !=None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        print evaluation.format_output(from_python(upval.pattern.expr)),\
                                eqs,evaluation.format_output(from_python(upval.replace)), '\n'
                print ""

        if definition.downvalues !=None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        print evaluation.format_output(from_python(downval.pattern.expr)),\
                            eqs,evaluation.format_output(from_python(downval.replace)), '\n'
                    print ""        
        return Symbol('Null');
        



class Information(PrefixOperator):
    """
    Retrieves information about symbols
    """
    operator="??"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold')
    
    def apply(self, symbol, evaluation):
        'Information[symbol_]'

        from mathics.core.expression import from_python        

        if type(symbol)!=Symbol: 
            print "Expression is not a symbol";
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None
   
        usagetext=_get_usage_string(symbol.name,evaluation);
        if usagetext!=None :
            print usagetext
        else:
            print definition.context+definition.name        
        print ""    

        if definition.ownvalues !=None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        print evaluation.format_output(from_python(ownval.pattern.expr)),\
                     eqs,evaluation.format_output(from_python(ownval.replace))
                print ""


        if definition.upvalues !=None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        print evaluation.format_output(from_python(upval.pattern.expr)),\
                                eqs,evaluation.format_output(from_python(upval.replace)), '\n'
                print ""

        if definition.downvalues !=None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        print "Built - in"
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        print evaluation.format_output(from_python(downval.pattern.expr)),\
                            eqs,evaluation.format_output(from_python(downval.replace)), '\n'
                    print ""
        attributesstr="{"
        for attr in definition.attributes:
            if  attributesstr!='{': attributesstr=attributesstr+", "
            attributesstr=attributesstr+attr
        attributesstr=attributesstr+"}"
        if attributesstr!="{}":         print "Attributes: ", attributesstr
        print ""

        print "options: ",definition.options
        print ""

#        print "messages: ",definition.messages
#        print ""
        return Symbol('Null');
        


