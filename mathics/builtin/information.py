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
    <dl>
    <dt>'Definition[$symbol$]'
        <dd>Retrieves the definition of $symbol$
    </dl>
    """
    operator="?"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold','Protect','ReadProtect')
    messages = {'notfound': 'Expression `1` is not a symbol'}
    def apply(self, symbol, evaluation):
        'Definition[symbol_]'
        if type(symbol)!=Symbol: 
            evaluation.message('Definition','notfound',symbol)            
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None

        textusage=_get_usage_string(symbol.name,evaluation)
        if textusage!=None:
            evaluation.print_out(String(textusage+"\n"))            
            return Symbol('Null')
        
        from mathics.core.expression import from_python        
        evaluation.print_out(String(definition.context+ symbol.name+"\n"))
        
        if definition.ownvalues !=None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        evaluation.print_out("Built - in")
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(ownval.pattern.expr)),\
                     eqs,evaluation.format_output(from_python(ownval.replace))))
                print ""


        if definition.upvalues !=None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        evaluation.print_out("Built - in")
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        evaluation.print_out(String(evaluation.format_output(from_python(upval.pattern.expr)),\
                                eqs,evaluation.format_output(from_python(upval.replace)), '\n'))
                

        if definition.downvalues !=None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        evaluation.print_out("Built - in")
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(downval.pattern.expr)),\
                            eqs,evaluation.format_output(from_python(downval.replace)), '\n'))                    
        return Symbol('Null');
        



class Information(PrefixOperator):    
    """
    <dl>
    <dt>'Information[$symbol$]'
        <dd>Retrieves information about $symbol$
    </dl>
    """
    operator="??"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold','Protect','ReadProtect')
    messages = {'notfound': 'Expression `1` is not a symbol'}
    
    def apply(self, symbol, evaluation):
        'Information[symbol_]'

        from mathics.core.expression import from_python        

        if type(symbol)!=Symbol: 
            evaluation.message('Information','notfound',symbol)                         
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None
   
        usagetext=_get_usage_string(symbol.name,evaluation);
        if usagetext!=None :
            evaluation.print_out(String(usagetext))
        else:
            print definition.context+definition.name        
        print ""    

        if definition.ownvalues !=None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(ownval.pattern.expr)),\
                     eqs,evaluation.format_output(from_python(ownval.replace))))
                print ""


        if definition.upvalues !=None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        evaluation.print_out(String(print "Built - in")
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        evaluation.print_out(String(evaluation.format_output(from_python(upval.pattern.expr)),\
                                eqs,evaluation.format_output(from_python(upval.replace)), '\n'))
                

        if definition.downvalues !=None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(downval.pattern.expr)),\
                            eqs,evaluation.format_output(from_python(downval.replace)), '\n'))
                    print ""
        attributesstr="{"
        for attr in definition.attributes:
            if  attributesstr!='{': attributesstr=attributesstr+", "
            attributesstr=attributesstr+attr
        attributesstr=attributesstr+"}"
        if attributesstr!="{}":         evaluation.print_out(String("Attributes: "+ attributesstr+"\n"))
        

        evaluation.print_out(String("options: "+definition.options+"\n"))
        

#        print "messages: ",definition.messages
#        print ""
        return Symbol('Null');
        


