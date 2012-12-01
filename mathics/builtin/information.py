# -*- coding: utf-8 -*-

"""
Information functions

Basic functions to get information about definitions in the current context.
"""



from mathics.builtin.base import Builtin, Predefined, BinaryOperator, PrefixOperator
from mathics.core.expression import Expression, Symbol,String
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
    >> ? Plus
         | System`Plus
         | Built - in


     >> B::usage="A short description of B"; ? B
         | A short description of B
    
    
    """
    operator="?"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold','Protect','ReadProtect')
    messages = {'notfound': 'Expression `1` is not a symbol'}
    def apply(self, symbol, evaluation):
        'Definition[symbol_]'
        if not isinstance(symbol,Symbol): 
            evaluation.message('Definition','notfound',symbol)            
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None

        textusage=_get_usage_string(symbol.name,evaluation)
        if textusage is not None:
            evaluation.print_out(String(textusage+"\n"))            
            return Symbol('Null')
        
        from mathics.core.expression import from_python        
        evaluation.print_out(String(definition.context+ symbol.name+"\n"))
        
        if definition.ownvalues is not None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
			evaluation.print_out(String(ownval.function.im_class.__doc__))
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(\
						from_python(ownval.pattern.expr))+\
							    eqs+evaluation.format_output(from_python(ownval.replace))+'\n'))



        if definition.upvalues is not None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        evaluation.print_out("Built - in")
			evaluation.print_out(String(upval.function.im_class.__doc__))
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        evaluation.print_out(String(evaluation.format_output(\
						from_python(upval.pattern.expr))+\
                                eqs+evaluation.format_output(from_python(upval.replace))+ '\n'))
                

        if definition.downvalues is not None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        evaluation.print_out("Built - in")
			evaluation.print_out(String(downval.function.im_class.__doc__))
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(\
					evaluation.format_output(\
						from_python(downval.pattern.expr))+\
						eqs+evaluation.format_output(from_python(downval.replace))+ '\n'))
        return Symbol('Null');
        



class Information(PrefixOperator):    
    """
    <dl>
    <dt>'Information[$symbol$]'
        <dd>Retrieves information about $symbol$
    </dl>

    >> ?? Plus
    | System`Plus
    | Built - in
    | Attributes: {Flat, Orderless, OneIdentity, Listable, Protected, NumericFunction}
    | 
    | Options[Plus] = {}
    | 
    | Messages[Plus] = {}
    | 
    

    >> B::usage="A short description of A"; ?? B
        | A short description of B
        | 
        | Options[B] = {}
        | 
        | Messages[B] = {HoldPattern[B::usage] :> A short description of B}
        | 
    
    """
    operator="??"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold','Protect','ReadProtect')
    messages = {'notfound': 'Expression `1` is not a symbol'}
    
    def apply(self, symbol, evaluation):
        'Information[symbol_]'

        from mathics.core.expression import from_python        

        if not isinstance(symbol,Symbol): 
            evaluation.message('Information','notfound',symbol)                         
            return Symbol('Null');

        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None
   
        usagetext=_get_usage_string(symbol.name,evaluation);
        if usagetext is not None :
            evaluation.print_out(String(usagetext))
        else:
            evaluation.print_out(String(definition.context+definition.name +"\n"))


        if definition.ownvalues is not None :
            if len(definition.ownvalues)!=0: 
                for ownval in definition.ownvalues:
                    if  type(ownval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
			evaluation.print_out(String(ownval.function.im_class.__doc__))
                    else:
                        if type(ownval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(ownval.pattern.expr))+\
                     eqs+evaluation.format_output(from_python(ownval.replace))+'\n'))



        if definition.upvalues is not None :
            if len(definition.upvalues)!=0: 
                for upval in definition.upvalues:
                    if  type(upval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
			evaluation.print_out(String(upval.function.im_class.__doc__))
                    else:
                        if type(upval)==RuleDelayed:
                            eqs=':^='
                        else:
                            eqs='^='
                        evaluation.print_out(String(evaluation.format_output(from_python(upval.pattern.expr))+\
                                eqs+evaluation.format_output(from_python(upval.replace))+ '\n'))
                

        if definition.downvalues is not None :
            if len(definition.downvalues)!=0: 
                for downval in definition.downvalues:
                    if  type(downval) == BuiltinRule:
                        evaluation.print_out(String("Built - in"))
			evaluation.print_out(String(downval.function.im_class.__doc__))
                    else:
                        if type(downval)==RuleDelayed:
                            eqs=':='
                        else:
                            eqs='='
                        evaluation.print_out(String(evaluation.format_output(from_python(downval.pattern.expr))+\
                            eqs+evaluation.format_output(from_python(downval.replace))+ '\n'))

        attributesstr="{"
        for attr in definition.attributes:
            if  attributesstr!='{': attributesstr=attributesstr+", "
            attributesstr=attributesstr+attr
        attributesstr=attributesstr+"}"
        if attributesstr!="{}":         evaluation.print_out(String("Attributes: "+ attributesstr+"\n"))
	evaluation.print_out(String(""))
	options=Expression('Options',symbol)
	options=Expression('Set',options,options.evaluate(evaluation))
	evaluation.print_out(options)
	evaluation.print_out(String(""))

	messag=Expression('Messages',symbol)
	messag=Expression('Set',messag,messag.evaluate(evaluation))
	evaluation.print_out(messag)
	evaluation.print_out(String(""))
#	optionsstr="{"
#        for opts in definition.options:
#           if  optionsstr!='{': 
#		    optionsstr+=", "
#            optionsstr+=evaluation.format_output(from_python(opts))+"->"+\
#		evaluation.format_output(from_python(definition.options[opts]))
#
#        optionsstr+="}"
#
#	if optionsstr!="{}":         
#		evaluation.print_out(String("Options: "+ optionsstr+"\n"))

        

#        print "messages: ",definition.messages
#        print ""
        return Symbol('Null');
        


