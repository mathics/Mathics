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
from mathics.builtin.patterns import match





class Definition(PrefixOperator):

    """
    Retrieves information about symbols
    """
    operator="?"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold')
    def apply(self, symbol, evaluation):
        'Definition[symbol_]'
        
        def out_callback(out):
            print unicode(out)
        from mathics.core.evaluation import Evaluation
            
#        evaluation = Evaluation(symbol.name+"::usage", evaluation.definitions, timeout=30, out_callback=out_callback)

#        for result in evaluation.results:
#            if result.result is not None:
#                print '%s' % unicode(result.result)
  
        def= evaluation.definitions.get_definition(symbol.name) ;
              
        print u"Calling Information for "+symbol.name;
        return None;
        

class Information(PrefixOperator):
    """
    Retrieves information about symbols
    """
    operator="??"
    precedence=1
    attributes = ('HoldAll', 'SequenceHold')
    
    def apply(self, symbol, evaluation):
        'Information[symbol_]'

        if type(symbol)!=Symbol: 
            print "Expression is not a symbol";
            return Symbol('Null');
        definition= evaluation.definitions.get_definition(symbol.name) ;
        if definition is None: return None
        print "Symbol:",definition.name        
        print ""    

        if definition.ownvalues !=None :
            if len(definition.ownvalues)!=0: print "ownvalues: ";
            for ownval in definition.ownvalues:
                if  type(ownval) == BuiltinRule:
                    print "Built - in"
                else:
                    print "\t",unicode(ownval.pattern.format(evaluation,'OutputForm')), \
                    ":=",unicode(ownval.replace.format(evaluation,'OutputForm'))
            print ""


        if definition.upvalues !=None :
            if len(definition.upvalues)!=0: print "upvalues: ";
            for upval in definition.upvalues:
                if  type(upval) == BuiltinRule:
                    print "Built - in"
                else:
                    print "\t",unicode(upval.pattern.format(evaluation,'OutputForm')), \
                    ":=",unicode(upval.replace.format(evaluation,'OutputForm'))
            print ""

        if definition.downvalues !=None :
            if len(definition.downvalues)!=0: print "downvalues: ";
            for downval in definition.downvalues:
                if  type(downval) == BuiltinRule:
                    print "Built - in"
                else:
                    print "\t",unicode(downval.pattern.expr.do_format(evaluation,'OutputForm')), \
                    ":=",unicode(downval.replace.do_format(evaluation,'OutputForm'))
            print ""

        print "attributes: ",definition.attributes
        print ""

        print "options: ",definition.options
        print ""

        print "messages: ",definition.messages
        print ""
        return None;
        



# class GetDefinition(Builtin):
#     operator = ''
#     precedence = 1
#     precedence_parse = None
#     needs_verbatim = False    
#     default_formats = True

#     def apply(self, symbol, evaluation):
#         print u"Calling Definition for "+symbol.name;
#         return Symbol('Infor');
        
        

