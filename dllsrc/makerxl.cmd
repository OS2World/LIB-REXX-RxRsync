/* This will create RxRSYNC.RXF : a set of RxRSYNC
procedure that can be loaded into Rexx MacroSpace.  
*/


foo=rxfuncquery('rexxlibregister')
if foo=1 then do
 say ' loading REXXLIB '
 call rxfuncadd 'rexxlibregister','rexxlib', 'rexxlibregister'
 call rexxlibregister
end
foo=rxfuncquery('rexxlibregister')
if foo=1 then do
   say "REXXLIB required to create macrospace procedures"
   exit
end /* do */

foo=macroclear()
if foo=0 then say "Warning: Problem clearing macrospace. "foo

foo=macroadd('Rsync_synopsis','rxrsync1.rex','b')
SAY FOO
foo=macroadd('Rsync_gdiff','rxrsync2.rex','b')
SAY FOO

foo=macroadd('Rsync_unGdiff','rxrsync3.rex','b')
SAY FOO

foo=macrosave('RxRSYNC.RXL')
if foo<>1 then
  say " Problem saving RXRSYNC.RXL (error= " foo
else
  say "success!"


