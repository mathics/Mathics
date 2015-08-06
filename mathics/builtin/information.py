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



class Information(PrefixOperator):    
    """
    <dl>
    <dt>'Information[$symbol$]'
        <dd>Prints information about a $symbol$
    </dl>

    >> Information[Plus]
     :| Built - in
     :| 
     :            <dl>
     :            <dt>'Plus[$a$, $b$, ...]'</dt>
     :            <dt>$a$ + $b$ + ...</dt>
     :                <dd>represents the sum of the terms $a$, $b$, ...
     :            </dl>
     :        
     :            >> 1 + 2
     :             = 3
     :        
     :            'Plus' performs basic simplification of terms:
     :        
     :            >> a + b + a
     :             = 2 a + b
     :            >> a + a + 3 * a
     :             = 5 a
     :            >> a + b + 4.5 + a + b + a + 2 + 1.5 b
     :             = 6.5 + 3. a + 3.5 b
     :        
     :            Apply 'Plus' on a list to sum up its elements:
     :            >> Plus @@ {2, 4, 6}
     :             = 12
     :            The sum of the first 1000 integers:
     :            >> Plus @@ Range[1000]
     :             = 500500
     :        
     :            'Plus' has default value 0:
     :            >> DefaultValues[Plus]
     :             = {HoldPattern[Default[Plus]] :> 0}
     :            >> a /. n_. + x_ :> {n, x}
     :             = {0, a}
     :        
     :            The sum of 2 red circles and 3 red circles is...
     :            >> 2 Graphics[{Red,Disk[]}] + 3 Graphics[{Red,Disk[]}]
     :             = 5 -Graphics-
     :        
     :            #> -2a - 2b
     :             = -2 a - 2 b
     :            #> -4+2x+2*Sqrt[3]
     :             = -4 + 2 Sqrt[3] + 2 x
     :            #> 2a-3b-c
     :             = 2 a - 3 b - c
     :            #> 2a+5d-3b-2c-e
     :             = 2 a - 3 b - 2 c + 5 d - e
     :        
     :            #> 1 - I * Sqrt[3]
     :             = 1 - I Sqrt[3]
     :        
     :            #> Head[3 + 2 I]
     :             = Complex
            
     : | {Flat, Listable, NumericFunction, OneIdentity, Orderless, Protected} = {Flat, Listable, NumericFunction, OneIdentity, Orderless, Protected}
     : | 
    """
    operator="??"
    precedence=1000
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
            evaluation.print_out(String(definition.name +"\n"))


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

        attributes=Expression('Attributes',symbol)
	attributes=attributes.evaluate(evaluation)
        if len(attributes.leaves)>0:
            attributes=Expression('Set',attributes,attributes.evaluate(evaluation))
            evaluation.print_out(attributes)
            evaluation.print_out(String(""))

	options=Expression('Options',symbol)
	options=options.evaluate(evaluation)	
	if len(options.leaves)>0:
	    options=Expression('Set',options,options.evaluate(evaluation))
	    evaluation.print_out(options)
	    evaluation.print_out(String(""))

	messag=Expression('Messages',symbol)
	messag=messag.evaluate(evaluation)
	if len(messag.leaves)>0:
	    messag=Expression('Set',messag,messag.evaluate(evaluation))
	    evaluation.print_out(messag)
	    evaluation.print_out(String(""))

        return Symbol('Null');
        


