# -*- coding: utf8 -*-

u"""
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

def test(definitions):
    from rules import Rule
    
    definitions.set_attribute('Plus', 'Flat')
    definitions.set_attribute('Plus', 'Orderless')
    definitions.set_attribute('g', 'Orderless')
    
    eval = Evaluation(Symbol('unused'), definitions)
    
    #expr = 'Plus[a,b,c,d,e,f,g,h,i,1,2,3]'
    #expr = 'a+b+a+e+a+b+a+e+a+b+a+e+a+b+a+e+f'
    #expr = 'a+b+c+d+e+f+g+h+i+j+k+l+a+b'
    #pattern = 'x_+x_+b+c+d+e+f+g+h+i+j+k+l'
    #expr = 'a+b+c+d+e+f+g+h+i+j+k+l+a+b+b+1+2+3'
    #pattern = 'x_+x_+c+d+e+f+g+h+i+j+k+l'
    #pattern = 'Plus[x_]'
    #expr = 'g[a+b+c+d+e,b,e,d,c,a]'
    #pattern = 'g[x__+y_,y_,x__]'
    #expr = 'g[a+b+c+d+e,b]'
    #pattern = 'g[x__+y_,y_]'
    #expr = 'a+a+b'
    #pattern = 'x__+x__'
    #expr = 'a+b+c'
    #pattern = 'x_+y_'
    expr = 'Format[a+b]'
    pattern = 'Format[Plus[items__]]'
    
    #expr = 'a*b+c*d+e*f+g*h+i*j+k*l+m*n'
    #pattern = 'a_*x_+b_*x_+a_*x_+b_*x_'
    
    expr = parse(expr).evaluate(eval)
    pattern = parse(pattern).evaluate(eval)
    print expr
    print pattern
    rule = Rule(pattern, parse('3'))
    #evaluation = Evaluation(Expression)
    print '%s' % rule.apply(expr, eval)
    
def test_parse():
    from parser import parser, Token
    
    print parser.parse([Token('-'), Token('parsedexpr', Expression('Pi', Symbol('Hey'))), Token('+'), Token('int', 2)])
