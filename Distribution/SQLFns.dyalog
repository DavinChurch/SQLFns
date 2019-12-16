:Namespace SQL
⍝ === VARIABLES ===

Config←(('s_Config[1] ← This description (for documentation only)',(⎕ucs 13),'s_Config[2] ← Valid characters that may be used in names (in addition to alphanumerics)',(⎕ucs 13),'s_Config[3] ← Quotation marks used to surround reserved/invalid names (e.g. ''""'' or ''[]'')',(⎕ucs 13),'s_Config[4] ← Quotation marks used to surround text values (e.g. "''''")',(⎕ucs 13),'s_Config[5] ← External wildcard character used to request SQL LIKE expressions (e.g. ''*'')',(⎕ucs 13),'s_Config[6] ← Nested vector of from-to pairs of substitution characters in names (e.g. ,⊂''∧ '')',(⎕ucs 13),'s_Config[7] ← (reserved)',(⎕ucs 13),'s_Config[8] ← (reserved)',(⎕ucs 13),'s_Config[9] ← List of SQL reserved words for this version of SQL',(⎕ucs 13))) (,'_') '""' '''''' '*' ⍬ ⍬ ⍬ ('ABORT' 'ABSOLUTE' 'ACCELERATED' 'ACTION' 'ADD' 'AFTER' 'ALL' 'ALLOCATE' 'ALTER' 'AND' 'ANY' 'ARE' 'AS' 'ASC' 'ASSERTION' 'AT' 'ATOMIC' 'AUTHORIZATION' 'AVG' 'AVOID' 'B-1' 'BEFORE' 'BEGIN' 'BETWEEN' 'BIT' 'BIT_LENGTH' 'BORDER' 'BOTH' 'BY' 'CALL' 'CASCADE' 'CASCADED' 'CASE' 'CAST' 'CATALOG' 'CHAR' 'CHARACTER' 'CHARACTER_LENGTH' 'CHAR_LENGTH' 'CHECK' 'CLOSE' 'COALESCE' 'COLLATE' 'COLLATION' 'COLUMN' 'COMMIT' 'COMMITTED' 'CONNECT' 'CONNECTION' 'CONSTRAINT' 'CONSTRAINTS' 'CONTINUE' 'CONVERT' 'CORRESPONDING' 'COUNT' 'CREATE' 'CREATESP' 'CREATETAB' 'CREATEVIEW' 'CROSS' 'CS' 'CURDATE' 'CURRENT' 'CURRENT_DATE' 'CURRENT_TIME' 'CURRENT_TIMESTAMP' 'CURRENT_USER' 'CURSOR' 'CURTIME' 'DATABASE' 'DATE' 'DAY' 'DCOMPRESS' 'DDF' 'DEALLOCATE' 'DEC' 'DECIMAL' 'DECIMALSEPARATORCOMMA' 'DECLARE' 'DEFAULT' 'DEFAULTCOLLATE' 'DEFERRABLE' 'DEFERRED' 'DELETE' 'DENY' 'DESC' 'DESCRIBE' 'DESCRIPTOR' 'DIAGNOSTICS' 'DICTIONARY' 'DISCONNECT' 'DISTINCT' 'DO' 'DOMAIN' 'DOUBLE' 'DROP' 'DSN' 'EACH' 'ELSE' 'END' 'END-EXEC' 'ENFORCED' 'ESCAPE' 'EX' 'EXCEPT' 'EXCEPTION' 'EXCLUSIVE' 'EXEC' 'EXECUTE' 'EXISTING' 'EXISTS' 'EXTERNAL' 'EXTRACT' 'FALSE' 'FETCH' 'FIRST' 'FLOAT' 'FN' 'FOR' 'FOREIGN' 'FOUND' 'FROM' 'FULL' 'FUNCTION' 'GET' 'GLOBAL' 'GO' 'GOTO' 'GRANT' 'GROUP' 'HANDLER' 'HAVING' 'HOUR' 'IDENTITY' 'IF' 'IMMEDIATE' 'IN' 'INDEX' 'INDICATOR' 'INITIALLY' 'INNER' 'INOUT' 'INPUT' 'INSENSITIVE' 'INSERT' 'INT' 'INTEGER' 'INTEGRITY' 'INTERNAL' 'INTERSECT' 'INTERVAL' 'INTO' 'IS' 'ISOLATION' 'JOIN' 'KEY' 'LANGUAGE' 'LAST' 'LEADING' 'LEAVE' 'LEFT' 'LEGACYOWNERNAME' 'LEVEL' 'LIKE' 'LOCAL' 'LOGIN' 'LOOP' 'LOWER' 'MASK' 'MATCH' 'MAX' 'MIN' 'MINUTE' 'MODE' 'MODIFIABLE' 'MODIFY' 'MODULE' 'MONTH' 'NAMES' 'NATIONAL' 'NATURAL' 'NCHAR' 'NEW' 'NEXT' 'NO' 'NORMAL' 'NOT' 'NOW' 'NULL' 'NULLIF' 'NUMERIC' 'OCTET_LENGTH' 'OF' 'OFF' 'OLD' 'ON' 'ONLY' 'OPEN' 'OPTION' 'OR' 'ORDER' 'OUT' 'OUTER' 'OUTPUT' 'OVERLAPS' 'OWNER' 'PAD' 'PAGESIZE' 'PARTIAL' 'POSITION' 'PRECISION' 'PREPARE' 'PRESERVE' 'PRIMARY' 'PRINT' 'PRIOR' 'PRIVILEGES' 'PROCEDURE' 'PUBLIC' 'READ' 'REAL' 'REFERENCES' 'REFERENCING' 'RELATIONAL' 'RELATIVE' 'RELEASE' 'REPEAT' 'REPEATABLE' 'REPLACE' 'RESTRICT' 'RETURN' 'RETURNS' 'REVOKE' 'RIGHT' 'ROLLBACK' 'ROW' 'ROWCOUNT' 'ROWS' 'SAVEPOINT' 'SCHEMA' 'SCROLL' 'SECOND' 'SECTION' 'SECURITY' 'SELECT' 'SERIALIZABLE' 'SESSION' 'SESSION_USER' 'SET' 'SIGNAL' 'SIZE' 'SMALLINT' 'SOME' 'SPACE' 'SQL' 'SQLCODE' 'SQLERROR' 'SQLSTATE' 'SSP_EXPR' 'SSP_PRED' 'START' 'SUBSTRING' 'SUM' 'SVBEGIN' 'SVEND' 'SYSTEM_USER' 'TABLE' 'TEMPORARY' 'THEN' 'TIME' 'TIMESTAMP' 'TIMEZONE_HOUR' 'TIMEZONE_MINUTE' 'TO' 'TOP' 'TRAILING' 'TRANSACTION' 'TRANSLATE' 'TRANSLATION' 'TRIGGER' 'TRIGGERSTAMPMISC' 'TRIM' 'TRUE' 'TRUEBITCREATE' 'TRUENULLCREATE' 'TS' 'UNCOMMITTED' 'UNION' 'UNIQUE' 'UNIQUEIDENTIFIER' 'UNKNOWN' 'UNTIL' 'UPDATE' 'UPPER' 'USAGE' 'USER' 'USER-DEFINED' 'USING' 'VALUE' 'VALUES' 'VARCHAR' 'VARYING' 'VIEW' 'WHEN' 'WHENEVER' 'WHERE' 'WHILE' 'WITH' 'WORDS' 'WORK' 'WRITE' 'YEAR' 'ZONE')

Config∆PSQL_ADO←(('s_Config[1] ← This description (for documentation only)',(⎕ucs 13),'s_Config[2] ← Valid characters that may be used in names (in addition to alphanumerics)',(⎕ucs 13),'s_Config[3] ← Quotation marks used to surround reserved/invalid names (e.g. ''""'' or ''[]'')',(⎕ucs 13),'s_Config[4] ← Quotation marks used to surround text values (e.g. "''''")',(⎕ucs 13),'s_Config[5] ← External wildcard character used to request SQL LIKE expressions (e.g. ''*'')',(⎕ucs 13),'s_Config[6] ← Nested vector of from-to pairs of substitution characters in names (e.g. ,⊂''∧ '')',(⎕ucs 13),'s_Config[7] ← (reserved)',(⎕ucs 13),'s_Config[8] ← (reserved)',(⎕ucs 13),'s_Config[9] ← List of SQL reserved words for this version of SQL',(⎕ucs 13))) (,'_') '""' '''''' '*' ⍬ ⍬ ⍬ ('ABORT' 'ABSOLUTE' 'ACCELERATED' 'ACTION' 'ADD' 'AFTER' 'ALL' 'ALLOCATE' 'ALTER' 'AND' 'ANY' 'ARE' 'AS' 'ASC' 'ASSERTION' 'AT' 'ATOMIC' 'AUTHORIZATION' 'AVG' 'AVOID' 'B-1' 'BEFORE' 'BEGIN' 'BETWEEN' 'BIT' 'BIT_LENGTH' 'BORDER' 'BOTH' 'BY' 'CALL' 'CASCADE' 'CASCADED' 'CASE' 'CAST' 'CATALOG' 'CHAR' 'CHARACTER' 'CHARACTER_LENGTH' 'CHAR_LENGTH' 'CHECK' 'CLOSE' 'COALESCE' 'COLLATE' 'COLLATION' 'COLUMN' 'COMMIT' 'COMMITTED' 'CONNECT' 'CONNECTION' 'CONSTRAINT' 'CONSTRAINTS' 'CONTINUE' 'CONVERT' 'CORRESPONDING' 'COUNT' 'CREATE' 'CREATESP' 'CREATETAB' 'CREATEVIEW' 'CROSS' 'CS' 'CURDATE' 'CURRENT' 'CURRENT_DATE' 'CURRENT_TIME' 'CURRENT_TIMESTAMP' 'CURRENT_USER' 'CURSOR' 'CURTIME' 'DATABASE' 'DATE' 'DAY' 'DCOMPRESS' 'DDF' 'DEALLOCATE' 'DEC' 'DECIMAL' 'DECIMALSEPARATORCOMMA' 'DECLARE' 'DEFAULT' 'DEFAULTCOLLATE' 'DEFERRABLE' 'DEFERRED' 'DELETE' 'DENY' 'DESC' 'DESCRIBE' 'DESCRIPTOR' 'DIAGNOSTICS' 'DICTIONARY' 'DISCONNECT' 'DISTINCT' 'DO' 'DOMAIN' 'DOUBLE' 'DROP' 'DSN' 'EACH' 'ELSE' 'END' 'END-EXEC' 'ENFORCED' 'ESCAPE' 'EX' 'EXCEPT' 'EXCEPTION' 'EXCLUSIVE' 'EXEC' 'EXECUTE' 'EXISTING' 'EXISTS' 'EXTERNAL' 'EXTRACT' 'FALSE' 'FETCH' 'FIRST' 'FLOAT' 'FN' 'FOR' 'FOREIGN' 'FOUND' 'FROM' 'FULL' 'FUNCTION' 'GET' 'GLOBAL' 'GO' 'GOTO' 'GRANT' 'GROUP' 'HANDLER' 'HAVING' 'HOUR' 'IDENTITY' 'IF' 'IMMEDIATE' 'IN' 'INDEX' 'INDICATOR' 'INITIALLY' 'INNER' 'INOUT' 'INPUT' 'INSENSITIVE' 'INSERT' 'INT' 'INTEGER' 'INTEGRITY' 'INTERNAL' 'INTERSECT' 'INTERVAL' 'INTO' 'IS' 'ISOLATION' 'JOIN' 'KEY' 'LANGUAGE' 'LAST' 'LEADING' 'LEAVE' 'LEFT' 'LEGACYOWNERNAME' 'LEVEL' 'LIKE' 'LOCAL' 'LOGIN' 'LOOP' 'LOWER' 'MASK' 'MATCH' 'MAX' 'MIN' 'MINUTE' 'MODE' 'MODIFIABLE' 'MODIFY' 'MODULE' 'MONTH' 'NAMES' 'NATIONAL' 'NATURAL' 'NCHAR' 'NEW' 'NEXT' 'NO' 'NORMAL' 'NOT' 'NOW' 'NULL' 'NULLIF' 'NUMERIC' 'OCTET_LENGTH' 'OF' 'OFF' 'OLD' 'ON' 'ONLY' 'OPEN' 'OPTION' 'OR' 'ORDER' 'OUT' 'OUTER' 'OUTPUT' 'OVERLAPS' 'OWNER' 'PAD' 'PAGESIZE' 'PARTIAL' 'POSITION' 'PRECISION' 'PREPARE' 'PRESERVE' 'PRIMARY' 'PRINT' 'PRIOR' 'PRIVILEGES' 'PROCEDURE' 'PUBLIC' 'READ' 'REAL' 'REFERENCES' 'REFERENCING' 'RELATIONAL' 'RELATIVE' 'RELEASE' 'REPEAT' 'REPEATABLE' 'REPLACE' 'RESTRICT' 'RETURN' 'RETURNS' 'REVOKE' 'RIGHT' 'ROLLBACK' 'ROW' 'ROWCOUNT' 'ROWS' 'SAVEPOINT' 'SCHEMA' 'SCROLL' 'SECOND' 'SECTION' 'SECURITY' 'SELECT' 'SERIALIZABLE' 'SESSION' 'SESSION_USER' 'SET' 'SIGNAL' 'SIZE' 'SMALLINT' 'SOME' 'SPACE' 'SQL' 'SQLCODE' 'SQLERROR' 'SQLSTATE' 'SSP_EXPR' 'SSP_PRED' 'START' 'SUBSTRING' 'SUM' 'SVBEGIN' 'SVEND' 'SYSTEM_USER' 'TABLE' 'TEMPORARY' 'THEN' 'TIME' 'TIMESTAMP' 'TIMEZONE_HOUR' 'TIMEZONE_MINUTE' 'TO' 'TOP' 'TRAILING' 'TRANSACTION' 'TRANSLATE' 'TRANSLATION' 'TRIGGER' 'TRIGGERSTAMPMISC' 'TRIM' 'TRUE' 'TRUEBITCREATE' 'TRUENULLCREATE' 'TS' 'UNCOMMITTED' 'UNION' 'UNIQUE' 'UNIQUEIDENTIFIER' 'UNKNOWN' 'UNTIL' 'UPDATE' 'UPPER' 'USAGE' 'USER' 'USER-DEFINED' 'USING' 'VALUE' 'VALUES' 'VARCHAR' 'VARYING' 'VIEW' 'WHEN' 'WHENEVER' 'WHERE' 'WHILE' 'WITH' 'WORDS' 'WORK' 'WRITE' 'YEAR' 'ZONE')

Config∆PSQL_ODBC←(('s_Config[1] ← This description (for documentation only)',(⎕ucs 13),'s_Config[2] ← Valid characters that may be used in names (in addition to alphanumerics)',(⎕ucs 13),'s_Config[3] ← Quotation marks used to surround reserved/invalid names (e.g. ''""'' or ''[]'')',(⎕ucs 13),'s_Config[4] ← Quotation marks used to surround text values (e.g. "''''")',(⎕ucs 13),'s_Config[5] ← External wildcard character used to request SQL LIKE expressions (e.g. ''*'')',(⎕ucs 13),'s_Config[6] ← Nested vector of from-to pairs of substitution characters in names (e.g. ,⊂''∧ '')',(⎕ucs 13),'s_Config[7] ← (reserved)',(⎕ucs 13),'s_Config[8] ← (reserved)',(⎕ucs 13),'s_Config[9] ← List of SQL reserved words for this version of SQL',(⎕ucs 13))) (,'_') '""' '''''' '*' ⍬ ⍬ ⍬ ('ABORT' 'ABSOLUTE' 'ACCELERATED' 'ACTION' 'ADD' 'AFTER' 'ALL' 'ALLOCATE' 'ALTER' 'AND' 'ANY' 'ARE' 'AS' 'ASC' 'ASSERTION' 'AT' 'ATOMIC' 'AUTHORIZATION' 'AVG' 'AVOID' 'B-1' 'BEFORE' 'BEGIN' 'BETWEEN' 'BIT' 'BIT_LENGTH' 'BORDER' 'BOTH' 'BY' 'CALL' 'CASCADE' 'CASCADED' 'CASE' 'CAST' 'CATALOG' 'CHAR' 'CHARACTER' 'CHARACTER_LENGTH' 'CHAR_LENGTH' 'CHECK' 'CLOSE' 'COALESCE' 'COLLATE' 'COLLATION' 'COLUMN' 'COMMIT' 'COMMITTED' 'CONNECT' 'CONNECTION' 'CONSTRAINT' 'CONSTRAINTS' 'CONTINUE' 'CONVERT' 'CORRESPONDING' 'COUNT' 'CREATE' 'CREATESP' 'CREATETAB' 'CREATEVIEW' 'CROSS' 'CS' 'CURDATE' 'CURRENT' 'CURRENT_DATE' 'CURRENT_TIME' 'CURRENT_TIMESTAMP' 'CURRENT_USER' 'CURSOR' 'CURTIME' 'DATABASE' 'DATE' 'DAY' 'DCOMPRESS' 'DDF' 'DEALLOCATE' 'DEC' 'DECIMAL' 'DECIMALSEPARATORCOMMA' 'DECLARE' 'DEFAULT' 'DEFAULTCOLLATE' 'DEFERRABLE' 'DEFERRED' 'DELETE' 'DENY' 'DESC' 'DESCRIBE' 'DESCRIPTOR' 'DIAGNOSTICS' 'DICTIONARY' 'DISCONNECT' 'DISTINCT' 'DO' 'DOMAIN' 'DOUBLE' 'DROP' 'DSN' 'EACH' 'ELSE' 'END' 'END-EXEC' 'ENFORCED' 'ESCAPE' 'EX' 'EXCEPT' 'EXCEPTION' 'EXCLUSIVE' 'EXEC' 'EXECUTE' 'EXISTING' 'EXISTS' 'EXTERNAL' 'EXTRACT' 'FALSE' 'FETCH' 'FIRST' 'FLOAT' 'FN' 'FOR' 'FOREIGN' 'FOUND' 'FROM' 'FULL' 'FUNCTION' 'GET' 'GLOBAL' 'GO' 'GOTO' 'GRANT' 'GROUP' 'HANDLER' 'HAVING' 'HOUR' 'IDENTITY' 'IF' 'IMMEDIATE' 'IN' 'INDEX' 'INDICATOR' 'INITIALLY' 'INNER' 'INOUT' 'INPUT' 'INSENSITIVE' 'INSERT' 'INT' 'INTEGER' 'INTEGRITY' 'INTERNAL' 'INTERSECT' 'INTERVAL' 'INTO' 'IS' 'ISOLATION' 'JOIN' 'KEY' 'LANGUAGE' 'LAST' 'LEADING' 'LEAVE' 'LEFT' 'LEGACYOWNERNAME' 'LEVEL' 'LIKE' 'LOCAL' 'LOGIN' 'LOOP' 'LOWER' 'MASK' 'MATCH' 'MAX' 'MIN' 'MINUTE' 'MODE' 'MODIFIABLE' 'MODIFY' 'MODULE' 'MONTH' 'NAMES' 'NATIONAL' 'NATURAL' 'NCHAR' 'NEW' 'NEXT' 'NO' 'NORMAL' 'NOT' 'NOW' 'NULL' 'NULLIF' 'NUMERIC' 'OCTET_LENGTH' 'OF' 'OFF' 'OLD' 'ON' 'ONLY' 'OPEN' 'OPTION' 'OR' 'ORDER' 'OUT' 'OUTER' 'OUTPUT' 'OVERLAPS' 'OWNER' 'PAD' 'PAGESIZE' 'PARTIAL' 'POSITION' 'PRECISION' 'PREPARE' 'PRESERVE' 'PRIMARY' 'PRINT' 'PRIOR' 'PRIVILEGES' 'PROCEDURE' 'PUBLIC' 'READ' 'REAL' 'REFERENCES' 'REFERENCING' 'RELATIONAL' 'RELATIVE' 'RELEASE' 'REPEAT' 'REPEATABLE' 'REPLACE' 'RESTRICT' 'RETURN' 'RETURNS' 'REVOKE' 'RIGHT' 'ROLLBACK' 'ROW' 'ROWCOUNT' 'ROWS' 'SAVEPOINT' 'SCHEMA' 'SCROLL' 'SECOND' 'SECTION' 'SECURITY' 'SELECT' 'SERIALIZABLE' 'SESSION' 'SESSION_USER' 'SET' 'SIGNAL' 'SIZE' 'SMALLINT' 'SOME' 'SPACE' 'SQL' 'SQLCODE' 'SQLERROR' 'SQLSTATE' 'SSP_EXPR' 'SSP_PRED' 'START' 'SUBSTRING' 'SUM' 'SVBEGIN' 'SVEND' 'SYSTEM_USER' 'TABLE' 'TEMPORARY' 'THEN' 'TIME' 'TIMESTAMP' 'TIMEZONE_HOUR' 'TIMEZONE_MINUTE' 'TO' 'TOP' 'TRAILING' 'TRANSACTION' 'TRANSLATE' 'TRANSLATION' 'TRIGGER' 'TRIGGERSTAMPMISC' 'TRIM' 'TRUE' 'TRUEBITCREATE' 'TRUENULLCREATE' 'TS' 'UNCOMMITTED' 'UNION' 'UNIQUE' 'UNIQUEIDENTIFIER' 'UNKNOWN' 'UNTIL' 'UPDATE' 'UPPER' 'USAGE' 'USER' 'USER-DEFINED' 'USING' 'VALUE' 'VALUES' 'VARCHAR' 'VARYING' 'VIEW' 'WHEN' 'WHENEVER' 'WHERE' 'WHILE' 'WITH' 'WORDS' 'WORK' 'WRITE' 'YEAR' 'ZONE')


⍝ === End of variables definition ===

(⎕IO ⎕ML ⎕WX)←1 1 3

∇ both←{left}And right;pp;addp;cnt
     ⍝∇ Join two (or more) phrases together with a SQL 'And' conjunction.
     ⍝ Handle cases where phrases may be empty.  (All-space phrases not supported.)
     ⍝ Allow argument(s) to be nested to specify multiple terms.
     ⍝ A special term of '()' causes non-empty results to be enclosed in parens.
     ⍝ A special term of '(,)' causes results to be enclosed in parens, but only if
     ⍝ there is more than one term being ANDed together.  This assumes that the
     ⍝ arguments are individual terms that haven't already been combined.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Modified 14 November 2014 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 :If 0=⎕NC'left' ⋄ left←'' ⋄ :EndIf ⋄ pp←'()' '(,)' ⋄ addp←0 0 ⋄ cnt←0 0
 :If 1<|≡left ⋄ addp∨←pp∊left ⋄ cnt[1]←⍴left←left~pp,⊂'' ⋄ left←⊃And/(⊂''),left ⋄ :EndIf
 :If 1<|≡right ⋄ addp∨←pp∊right ⋄ cnt[2]←⍴right←right~pp,⊂'' ⋄ right←⊃And/(⊂''),right ⋄ :EndIf
 addp∨←pp∊left right ⋄ left←(~(⊂left)∊pp)/left ⋄ right←(~(⊂right)∊pp)/right ⋄ cnt+←(cnt=0)××≢¨left right
 both←left,((∧/×(⍴left),⍴right)/' And '),right
 :If ∨/addp∧(×⍴both),1<+/cnt ⋄ both←'()'∆Q both ⋄ :EndIf
∇

∇ sql←Cmd clauses;fn;dyad;left;right
     ⍝∇ Generate any of several SQL commands by providing only a data structure
     
     ⍝ If the right argument is unnested, assume it's a finished SQL command and just return it as-is
     ⍝ If it's nested, assume it's the right argument to Get (the most common operation) -- UNLESS:
     ⍝ if the first simple character = '∇', then we have a special case as follows:
     ⍝ The first item of the nested vector is the name of the SQL-formatting function to be called,
     ⍝ with a '∇' added as the first character.  This is usually one of:
     ⍝       ∇Get, ∇Upd, ∇Ins, ∇Del
     ⍝ In such a case, the named subroutine is called and the remainder of the nested vector is
     ⍝ passed to it as a right argument.
     ⍝ Optionally, a left argument may also be passed to the subroutine.  In this case, start the
     ⍝ function name (after the '∇') with the '⍺' character.  This means that the next item in the
     ⍝ argument is to be passed as the left argument of the function call and any remainder is to be
     ⍝ passed as the right argument.  If more than one '⍺' is prepended, then multiple items are
     ⍝ taken from the input and used as the left argument.  (A single item is unnested before it is
     ⍝ passed.)  Mostly for future support, a '⍵' suffix character is also allowed to be appended
     ⍝ to the end of the function name to indicate that that many items of the input are to be passed
     ⍝ as a right argument to the subroutine, in the same way as the left argument items.
     
     ⍝ Written 16 December 2014 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡sql←clauses ⋄ :Return ⋄ :EndIf ⋄ left←right←⍬
 :If '∇'∊⊃⊃clauses ⋄ fn←(⊃clauses)~'∇' ⋄ clauses←1↓clauses ⋄ :Else ⋄ fn←'Get' ⋄ :EndIf
 :If dyad←'⍺'∊fn
     left←(+/fn∊'⍺')↑clauses ⋄ clauses←(⍴left)↓clauses
     :If 1=⍴left ⋄ left←⊃left ⋄ :EndIf ⋄ fn~←'⍺'
 :EndIf
 right←clauses ⍝ Anything remaining is assumed to be the function's right argument
 :If '⍵'∊fn
     right←(+/fn∊'⍵')↑clauses ⋄ clauses←(⍴right)↓clauses
     :If 1=⍴right ⋄ right←⊃right ⋄ :EndIf ⋄ fn~←'⍵'
 :EndIf
     ⍝ Now call the subroutine they requested (assuming there's no local-variable shadowing)
 :Trap 0 ⋄ sql←⍎(dyad/'left '),fn,' right' ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
     ⍝? Allow for an '⍎' option to run arbitrary code??
∇

∇ sql←Del clauses;from;where
     ⍝∇ Generate a SQL Delete command from it's two component clauses by passing them
     ⍝∇ to the Delete & Where functions and catenating the results together.
     ⍝
     ⍝ Provide arguments in this order (the standard order of a SQL Delete statement):
     ⍝       {from-table} {where-clause}
     ⍝ (The where-clause is required to avoid accidental deleting of entire tables.)
     ⍝
     ⍝ Written 16 December 2014 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 'Length Error'⎕SIGNAL 5/⍨(2≤|≡clauses)∧2≠¯1↑1,⍴clauses ⋄ from where←clauses
 :Trap 0
     'No Where clause constructed'⎕SIGNAL 5/⍨0∊⍴sql←Where where
     sql←(Delete from),sql
 :Else
     ⎕SIGNAL ∆dmx
 :EndTrap
∇

∇ sql←Delete table
     ⍝∇ Produce a SQL DELETE clause on a single table name.
     ⍝ (Don't forget to add a WHERE clause to keep from emptying the table.)
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡table ⋄ :If 2=⍴⍴table ⋄ table←↓table ⋄ :Else ⋄ table←⊂table ⋄ :EndIf ⋄ :EndIf
 table←(∆dlt¨,,¨table)~⊂''
 'Missing table name to delete from'⎕SIGNAL 11/⍨0∊⍴table
 'Cannot Delete from joined tables'⎕SIGNAL 11/⍨1<≢table
 'Cannot Delete from joined tables'⎕SIGNAL 11/⍨∨/'→><≠∘='∊⊃table
 'Cannot Delete from joined tables'⎕SIGNAL 11/⍨∨/'(,)'∊⊃table
 :Trap 0 ⋄ sql←'Delete',From table ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
∇

∇ value←default Empty data
     ⍝∇ Provide a default "empty value" structure when input is empty.
     ⍝ Returns right argument intact, unless 0∊⍴data in which case return left argument.
     
 :If 0∊⍴value←data ⋄ value←default ⋄ :EndIf
∇

∇ sql←{default}From tables;joins;aliases;join;symbols;t;j;table;tbls;on;included;alias;joinnames;n;name;s;nested;j1;j2;before;query;maintenance;fields;x;tmatch;fmatch;jointype;sfd;deletion;jj;parsing;tp;extra
     ⍝∇ Generate a SQL "From" clause by providing shorthand notation for easier
     ⍝∇ writing and reading of code (particularly when it's complex).
     ⍝
     ⍝   {fromclause} ← From {join1} [join2] [join3] ...
     ⍝
     ⍝ The {join} arguments each specify one table connection to another table.
     ⍝ Multiple {join} arguments may be required to join more than two tables together.
     ⍝ They are given as a nested list of character vectors, each in one of
     ⍝   several possible formats (see examples below).
     ⍝ Table names in the examples below are shown as letters (e.g. A, B, C...) instead of real table names.
     ⍝ Field names in the examples below are shown as digits (e.g. 1, 2, 3...) intead of real field names.
     ⍝   A               Specifies an unjoined table name (or implies a separate implicit join)
     ⍝   A=B             Natural Join table A to table B
     ⍝   A(1)→B(2)       Inner Join table A to B On A.1=B.2
     ⍝   X←A∘B           Cross Join tables A & B, but giving A an alias name of X
     ⍝   A(1)<Y←B(2)     A Right Outer Join from A to B, but B gets an alias
     ⍝   X←A(1)>Y←A(2)   A Left Outer self-join from A to itself, using X & Y as aliases
     ⍝   A(1,2)→B(3,4)   A two-field Inner Join On A.1=B.3 And A.2=B.4
     ⍝   A→B             Join A to B using previously defined join criteria                                ?
     ⍝   A()→B()         Join A to B ignoring any previously defined join criteria
     ⍝
     ⍝ The APL assignment arrow ("←") is used to specify aliases for any table name, as in:
     ⍝   X←A (X will be used as an alias for real table name A)
     ⍝   When using aliases, either:
     ⍝   (1) Use the entire alias specification for every occurance of that table name
     ⍝       reference (e.g. X←A everywhere), or
     ⍝   (2) Use the alias specification for one occurance of the table name and then
     ⍝       use the alias name alone for the other references to that table.
     ⍝   Use of the real table name a second time (alone or with a different alias)
     ⍝   will cause a second copy of the table to be joined into the view.
     ⍝   For example:  'X←A→B' 'X→C' and 'X←A→B' 'X←A→C' both use the same copy of "A",
     ⍝           but   'X←A→B' 'A→C' and 'A→B'   'X←A→C' both join in two copies of "A".
     ⍝
     ⍝ The symbols that are used to separate the table names specify the type
     ⍝   of join to be performed:
     ⍝   → Inner Join                (a very common type of join)
     ⍝   > Left Outer Join           (a common type of join)
     ⍝   < Right Outer Join
     ⍝   ≠ Full Outer Join
     ⍝   ∘ Cross (Cartesian product) Join
     ⍝   = Natural Join
     ⍝ For all joins except the Cross Join and the Natural Join, corresponding
     ⍝   pairs of fields to use for the join are required.
     ⍝ These should be specified in parentheses, after the table name where they
     ⍝   are found, and multiple field names are separated by commas.
     ⍝ Always specify the join-from table on the left and the join-to table
     ⍝   on the right -- join symmetry is not supported by this function.
     ⍝ Joining fields may be globally defaulted for particular table pairs by using
     ⍝   an auto-defaulting mechanism built into this function (see below).  This
     ⍝   will enable field-less join specifications for many uses of From.
     ⍝
     ⍝ A pair of table names listed in this fashion may be augmented by including
     ⍝ additional custom criteria.  This allows join conditions besides just one-for-one
     ⍝ field name matching.  To specify this, replace the character vector listing the
     ⍝ tables (as above) with a nested vector, the first item of which is the replaced
     ⍝ vector.  Any additional items in this nested vector are additional join terms,
     ⍝ each of which can be specified in any structure suitable for use by Where.
     ⍝ In the rare case when ONLY custom criteria is to be used, specify no field names
     ⍝ between the parentheses.  If there is a need to specify such a custom-only join
     ⍝ criteria alongside other normal field-based joins, then use field names beginning
     ⍝ with "⍝" to allow the special joins to be distinguished from the normal ones by
     ⍝ using these "commented" names.  The commented names will be used only for this
     ⍝ disambiguation and will not be returned as part of the actual join conditions.
     ⍝ For example:
     ⍝   From ⊂'A(1)→B(2)' ('A.3' Is 'Admin')
     ⍝   From ⊂'A(⍝1)→B(⍝2)' ('⍎A.1+A.2' Is '⍎B.1+B.2')
     ⍝
     ⍝ "Chained" joins may also be specified (when using defaults) by including
     ⍝ multiple table names in a single string, separated by appropriate join symbols.
     ⍝ For example, A→B→C→D joins four tables together using their default criteria.
     ⍝
     ⍝ *** To define defaults, call From with a left argument, as in:
     ⍝   '+' Add new join default(s) to the global table
     ⍝   '←' Empty the global table, and also add joins to the table (if any are given)
     ⍝   '-' Remove existing join default(s) from the global table
     ⍝   '?' Display (some) join default(s) in the global table
     ⍝ The right argument is the same as described above
     ⍝   (although some combinations won't make much sense to use).
     ⍝ If the left argument is '?', the right argument may contain table names, and
     ⍝   only default-join information that involves those table(s) will be returned.
     ⍝   If no table names are provided, then the entire default list is returned.
     ⍝
     ⍝ *** For utility use, to parse and return table and field names, call From with a left argument of '⊂'.
     ⍝ The returned structure is a nested matrix with one row per provided table name, as follows:
     ⍝   [;1] From table name
     ⍝   [;2] Join type (a single character; ' ' for an unjoined table)
     ⍝   [;3] To table name (or '' for an unjoined table)
     ⍝   [;4] Nested matrix of field names used for this join, one row per join criteria (field-pair):
     ⍝        [;1] From field name
     ⍝        [;2] '=' (or '⍵' for custom where-clause joining)
     ⍝        [;3] To field name
     ⍝
     ⍝ Subroutines required: And, Q, Where, ∆Q, ∆cut, ∆cuts, ∆dlt, ∆sew, ∆uc, and their subroutines.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Last modified 22 August 2018 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 :If query←deletion←parsing←maintenance←×⎕NC'default'
     query deletion parsing←(⊂⊂default)∊¨(⊂⍬ 1)⍴¨¨'?←⊂'
 :EndIf
 :If 1≥|≡tables ⋄ :If 2=⍴⍴tables ⋄ tables←↓tables ⋄ :Else ⋄ tables←⊂tables ⋄ :EndIf ⋄ :EndIf
 tables←(∆dlt¨,,¨tables)~⊂''
     
 joins←0 4⍴0 ⋄ aliases←0 2⍴0 ⋄ symbols←'→><≠∘='
 joinnames←'Inner Join' 'Left Outer Join' 'Right Outer Join'
 joinnames←joinnames,'Full Outer Join' 'Cross Join' 'Natural Join'
     
 :For join :In tables
     extra←⍬ ⍝ Allow extra Where criteria to be included for non-standard joins
     :If 1<|≡join ⋄ extra←1↓join ⋄ join←⊃join ⋄ :EndIf ⍝ Store for later use
     :If ∨/symbols∊join
         :While ∨/t←join∊symbols
             j←(¯1+(+\t)⍳2)↑join ⋄ join←(t⍳1)↓join ⋄ jointype←⊃(j∊symbols)/j
             :If ∨/0∊¨⍴¨tbls←∆dlt¨jointype ∆cuts j ⋄ 'Missing table name'⎕SIGNAL 11 ⋄ :EndIf
             :For t :In (tp←∨/¨(⊂'()')∊¨tbls)/⍳⍴tbls ⍝ Validate () syntax
                 :If ~∧/1=+/t←'()'∘.=j←∆dlt t⊃tbls ⋄ :OrIf ∨/0>-⌿+\t ⋄ :OrIf ')'≠¯1↑j
                     ('Invalid parentheses in table specification: ',j)⎕SIGNAL 11
                 :EndIf
             :EndFor
             tbls←∆dlt¨¨2↑¨(⊂'()')∆cuts¨tbls
             alias←2⍴¨∆dlt¨¨'←'∆cut¨⊃¨tbls
             :If ×≢t←⊃(((∆uc alias)⍳∆uc alias)=⍳⍴alias)/alias
             :AndIf ×≢t←(≢/∆uc t)⌿t
             :AndIf ×≢t←(~∨⌿(∆uc aliases⍪⊂'')∧.≡⍉∆uc t)⌿t
                 :If ∨/t[;1]∊aliases[;1] ⋄ 'Multiple uses of the same alias'⎕SIGNAL 11 ⋄ :EndIf
                 aliases⍪←t
             :EndIf
             on←∆dlt¨¨','∆cut¨∆dlt¨2⊃¨tbls
             :If ∨/~tp                       ⍝ If field name lists were not provided...
             :AndIf jointype∊¯2↓symbols      ⍝ ...and it requires field names.
                 :If ~query ⍝ Queries can be on tables only.
                     :If 0=⎕NC'From_Defaults'
                     :OrIf 0∊⍴sfd←From_Defaults
                     :OrIf 0∊⍴sfd←(((∆uc sfd[;1])∊⊂∆uc 1 2⊃alias)∧(∆uc sfd[;3])∊⊂∆uc 2 2⊃alias)⌿sfd
                     :OrIf 0∊⍴sfd←(((~tp[1])∧0∊⍴1⊃on)∨∧/¨(∆uc('⍵'≠2⌷[2]¨sfd[;4])/¨(1⌷[2]¨sfd[;4])~¨¨'⍝')∊⊂∆uc(1⊃on)~¨'⍝')⌿sfd ⍝ Left Disambiguation
                     :OrIf 0∊⍴sfd←(((~tp[2])∧0∊⍴2⊃on)∨∧/¨(∆uc('⍵'≠2⌷[2]¨sfd[;4])/¨(3⌷[2]¨sfd[;4])~¨¨'⍝')∊⊂∆uc(2⊃on)~¨'⍝')⌿sfd ⍝ And right disambiguation
                         'No default join found'⎕SIGNAL 11
                     :ElseIf 1<≢sfd
                         'Ambiguous join conditions found'⎕SIGNAL 11
                     :EndIf
                     :If 0∊⍴on←⊃sfd[1;4] ⋄ 'Default join is missing field names'⎕SIGNAL 11 ⋄ :EndIf
                 :EndIf
             :ElseIf ∨/tp                    ⍝ If any field name lists are present...
             :AndIf jointype∊¯2↑symbols      ⍝ ...and it requires that field names be absent.
                 ('Field indexes not permitted for ',(symbols⍳jointype)⊃joinnames)⎕SIGNAL 11
             :EndIf
             :If 2≠⍴⍴on ⍝ "on" is already a matrix if we read it out of the global
                 :If ≠/≢¨on ⋄ 'Field count mismatch'⎕SIGNAL 11 ⋄ :EndIf
                 on←(1⊃on),'=',[1.5]2⊃on
             :EndIf
             :If ×⍴extra
                      ⍝ Fail if any extra criteria are not suitable for use by Where
                 :Trap 0 ⋄ t←Where¨extra ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
                 on⍪←(⊂''),'⍵',[1.5]extra ⍝ Add directly to join terms as-is
                 :If ∨/symbols∊join ⍝ Are there any more (chained) joins in this term?
                     'Custom join criteria not allowed on chained joins'⎕SIGNAL 11
                 :EndIf
             :EndIf
             :If (0∊⍴on)∧jointype∊¯2↓symbols
                 'No required join criteria specified'⎕SIGNAL 11
             :EndIf
             joins⍪←(∆dlt⊃1⊃alias)jointype(∆dlt⊃2⊃alias)on
         :EndWhile
     :Else
         :If query
                 ⍝ Do something special for the querying process.
             table fields←∆dlt¨2↑'()'∆cut join ⋄ table←⊃⌽'←'∆cut table ⍝ Ignore aliases.
             x←⍪∆dlt¨','∆cut fields ⋄ x←x,((≢x),2)⍴⊂''
             joins⍪←table',' ''x
         :ElseIf maintenance∧~parsing
             'Invalid structure for defaulting'⎕SIGNAL 11
         :Else
             :If ∨/'(,)'∊join ⋄ 'Invalid argument'⎕SIGNAL 11 ⋄ :EndIf
                 ⍝ Append a single line for an independent table
             :If ≢/alias←2⍴∆dlt¨'←'∆cut join
             :AndIf ~∨/(aliases⍪⊂'')∧.≡alias
                 :If alias[1]∊aliases[;1] ⋄ 'Multiple uses of the same alias'⎕SIGNAL 11 ⋄ :EndIf
                 aliases⍪←alias
             :EndIf
             joins⍪←(∆dlt⊃alias)',' ''(0 3⍴⊂'')
         :EndIf
     :EndIf
 :EndFor
     
 :If (~query∨deletion)∧0∊⍴joins ⋄ 'No From information provided'⎕SIGNAL 11 ⋄ :EndIf
 t←' '=⊃0⍴⊂∊x←↓joins ⋄ (t/∊x)←∆uc t/∊x ⋄ 'Duplicate join conditions'⎕SIGNAL 11/⍨∨/(x⍳x)≠⍳⍴x
 t←joins[;2]=',' ⋄ 'Multiple references to the same table'⎕SIGNAL 11/⍨∨/(↑⌊/(↓[1]∆uc joins[;1 3])⍳¨⊂∆uc t/joins[;1])<t/⍳≢joins
     
     ⍝ Internal notes:
     ⍝ The "joins" variable:
     ⍝ Each row is 1 item of the join
     ⍝   [;1]  From Table (vector of table names)
     ⍝   [;2]  Join Type (if this is a comma, then all we have is a table name in column 3)
     ⍝   [;3]  To Table (vector of table names)
     ⍝   [;4]  "On" clause:  a 3-column matrix of:
     ⍝           [;1]  From Field (corresponds to From table)
     ⍝           [;2]  Comparison operator (usually '='; '⍵' if extra criteria)
     ⍝           [;3]  To Field (corresponds to To table)
     
     ⍝ Order-of-precedence examples:
     ⍝   Case 1:  A→B                     A→B
     ⍝   Case 2:  A→B;B→C                (A→B)→C
     ⍝   Case 3:  A→C;B→A                (B→A)→C
     ⍝   Case 4:  A→B;B→C;C→D;B→D;A→D   ((A→B)→C)→D
     
 :If maintenance
         ⍝ They're asking to modify/query the global default join schema,
         ⍝ rather than to produce a SQL From clause.
         ⍝ No aliases allowed; we just can't handle the alias.
     :If deletion∨0=⎕NC'From_Defaults' ⋄ From_Defaults←0 4⍴⊂'' ⋄ :EndIf
         ⍝ Convert alias names back to original table names for maintenance purposes
     t←aliases⍪2/⍪,joins[;1 3] ⋄ joins[;1 3]←t[t[;1]⍳joins[;1 3];2]
     :Select default
     :CaseList ⍬ 1∘.⍴'+←' ⍝ Add these joins to the default table
         From_Defaults←((~(↓∆uc From_Defaults[;1 3 4])∊↓∆uc joins[;1 3 4])⌿From_Defaults)⍪joins
     :CaseList ⍬ 1⍴¨'-'   ⍝ Remove these joins from the default table (must match exactly)
         From_Defaults←(~(↓∆uc From_Defaults[;1 3 4])∊↓∆uc joins[;1 3 4])⌿From_Defaults
     :CaseList ⍬ 1⍴¨'?'   ⍝ Format & return all or part of the default table
             ⍝ Limit results based on what they asked for
         :If (×≢joins←∆uc joins)∧×≢jj←∆uc j←From_Defaults
             joins[;4]~¨¨←'⍝' ⋄ jj[;4]~¨¨←'⍝' ⍝ Allow commented fields to be matched
             tmatch←joins[;1]∘.≡jj[;1]
             tmatch∨←t←(joins[;1]∘.≡jj[;3])∧[1]joins[;2]=','
             tmatch∧←(joins[;3]∘.≡jj[;3])∨[1]joins[;2]=','
             fmatch←(~t)∧∧/¨(1⌷[2]¨joins[;4])∘.∊1⌷[2]¨jj[;4]
             fmatch∨←t∧(∧/¨(1⌷[2]¨joins[;4])∘.∊3⌷[2]¨jj[;4])∧[1]joins[;2]=','
             fmatch∧←(∧/¨(3⌷[2]¨joins[;4])∘.∊3⌷[2]¨jj[;4])∨[1]joins[;2]=','
             j←(∨⌿tmatch∧fmatch)⌿j
         :EndIf
             ⍝ Format the joins back the way they gave 'em to us
         :If ×≢j
             t←'⍵'=2⌷[2]¨j[;4] ⋄ j1←(~t)⌿¨j[;4] ⋄ j2←t⌿¨j[;4]
             j,←','∆sew¨1⌷[2]¨j1 ⋄ j,←','∆sew¨3⌷[2]¨j1
             j[;1],¨←(×≢¨j[;5])/¨'()'∆Q j[;5]
             j[;3],¨←(×≢¨j[;6])/¨'()'∆Q j[;6]
             sql←j[;1],¨(','≠j[;2])/¨j[;2],¨j[;3]
             :If ∨/t←×≢¨j2 ⋄ (t/sql)←(⊂¨t/sql),¨3⌷[2]¨t/j2 ⋄ :EndIf
         :Else
             sql←0⍴⊂''
         :EndIf
     :CaseList ⍬ 1⍴¨'⊂'   ⍝ Return parsed join-list, in internal format (almost)
         sql←joins ⍝ We mostly give it to them back like this
         ((sql[;2]=',')/sql[;2])←' ' ⍝ Convert ',' to more readable ' ' for them
     :Else
         'Invalid default-editing command code'⎕SIGNAL 11
     :EndSelect
     :Return ⍝ Just doing defaults management, so not returning a SQL command
 :EndIf
     
 sql←0⍴⊂'' ⍝ Where-terms for the final SQL command
 :While ×≢joins
     j←<\~joins[;1]∊joins[;3] ⍝ Pick a table name to start with
     'Recursive join sequence'⎕SIGNAL 11/⍨~∨/j
     n←'.'Q name←⊃j/joins[;1] ⋄ included←,⊂name
     :If (⊂name)∊aliases[;1] ⍝ Provide alias name
         n←('.'Q⊃aliases[aliases[;1]⍳⊂name;2]),' As ',n
         aliases←(~aliases[;1]∊⊂name)⌿aliases ⍝? May not be needed for algorithm?
     :EndIf
     :If ','=⊃j/joins[;2] ⋄ joins←(~j)⌿joins ⋄ :EndIf
     s←n ⋄ nested←0 ⍝ Start the Join term with a table name
     :While ~∧/1,joins[;2]=',' ⍝ Stop looping if no more explicitly-joined tables exist
         j1←(','≠joins[;2])∧(∆uc joins[;3])∊∆uc included ⍝ Antecedents satisfied?
         j1∧←(~joins[;1]∘.≡joins[;1])∧.∨(∆uc joins[;3])∊∆uc included
         j2←(','≠joins[;2])∧(∆uc joins[;1])∊∆uc included ⍝ Precedents satisfied?
         j2∧←(~joins[;3]∘.≡joins[;3])∧.∨(∆uc joins[;1])∊∆uc included
         j←<\j1∨j2 ⍝ Preferential order is the order they were given to us
         :If ~∨/j ⋄ :Leave ⋄ :EndIf ⍝ No more can be included in this term
         before←∨/j∧j1 ⍝ Antecedents were satisfied first?
             ⍝? Not if sure antecedent precedence is possible any more??
         jointype←(symbols⍳⊃j/joins[;2])⊃joinnames
         n←'.'Q name←⊃j/joins[;3]
         'Table re-used'⎕SIGNAL 11/⍨(⊂∆uc name)∊∆uc included ⍝ (Debugging)
         included,←⊂name
         :If (⊂name)∊aliases[;1] ⍝ Provide alias name
             n←('.'Q⊃aliases[aliases[;1]⍳⊂name;2]),' As ',n
             aliases←(~aliases[;1]∊⊂name)⌿aliases ⍝? May not be needed for algorithm?
         :EndIf
         s←((nested>0)/'()')∆Q s ⋄ nested←nested+1 ⍝ Use parens with >2 tables
         :If before ⋄ s,⍨←n,' ',jointype,' ' ⍝ Antecedents satisfied first
         :Else ⋄ s,←' ',jointype,' ',n ⍝ Precendents satisfied first
         :EndIf
         :If (⊂jointype)∊¯2↓joinnames ⍝ Do we need an 'On' sub-clause?
             j←joins[;3]∊j/joins[;3]
             on←⊃⍪/j/(joins[;4],¨⊂¨joins[;1]),¨⊂¨joins[;3]
             on←(~∨/('⍝'∊¨on[;1 3])∧on[;2 2]≠'⍵')⌿on ⍝ Ignore commented fields
             :If ∨/t←on[;2]='⍵' ⋄ (t⌿on[;3])←7↓¨Where¨t⌿on[;3] ⋄ (t/on[;2])←⊂'' ⋄ :EndIf
             :If ×⍴t←(~t)/⍳⍴t
                 on[t;1 3 4 5]←Q on[t;1 3 4 5]
                 on[t;2]←' ',¨on[t;2],¨' ' ⋄ on[t;1 3]←on[t;4 5],¨'.',¨on[t;1 3]
             :EndIf
             s←s,(×≢on)/' On ',And ∆sew on[;⍳3] ⍝ Build the join condition
         :ElseIf ×≢⊃j/joins[;4]
             'Index fields not allowed'⎕SIGNAL 11
         :EndIf
         joins←(~j)⌿joins ⍝ This Join is done - remove it from further consideration
     :EndWhile
     sql←sql,⊂s ⍝ Another "From" term
         ⍝? If this error *only* occurs due to bad input, should we return a different formal message?
         ⍝? If it might occur elsewhere, can we pre-test for that condition (e.g. A→C;B→C)?
         ⍝? Or should we go ahead and try to auto-reverse the table names (and join direction) for them?
         ⍝? Prescan and have two different error messages; do not auto-reverse.  ⍝? OK now?
     'Table not completely handled in one place'⎕SIGNAL 11/⍨∨/∨/(∆uc joins[;1 3])∊∆uc included ⍝ (Debugging)
 :EndWhile
     
 sql←' From ',', '∆sew sql
     
     ⍝? Possible future thoughts:
     ⍝  - Hints?
     ⍝  - Using?
     ⍝  - Consider the possibility of specifying 'A,B' and letting the default table figure out
     ⍝    not only what fields to match on but also figure out what type of join to use.
     ⍝    In fact, allowing 'A,B,C' to be the same as ,¨'A' 'B' 'C' in general sounds good.
     ⍝  - What about allowing fields or table.fields to work with "?"?
∇

∇ sql←{options}Get clauses;select;from;where;order;group;having;unionall;t;quest
     ⍝∇ Generate a simple Select command using the items of the argument given
     ⍝∇ to pass to the subroutines Select, From, Where, OrderBy, GroupBy, & Having
     ⍝∇ and return the results catenated together into a whole SQL command.
     ⍝ Provide arguments in this order (the standard order of a SQL Select statement):
     ⍝       {select-fields} {from-tables} [where-terms] [order-fields] [group-fields] [having-fields]
     ⍝
     ⍝ Get can also take a nested matrix of arguments, where each row of the matrix is a separate
     ⍝ query in the above pattern.  A Select/Union statement will then be generated as the result
     ⍝ of Get.  Only the first three columns may be different in each row. [order-fields] may only
     ⍝ be specified once for the entire SQL command, so it should either appear only in one of the
     ⍝ rows (the remainder being empty) or it should be exactly duplicated in all rows in which it
     ⍝ appears.  [group-fields] and [having-fields] are not supported in Select/Union SQL commands.
     ⍝ The {select-fields} list from every row must produce the same number and type of fields.
     ⍝ By default, duplicate rows (from different queries) may be returned from the resulting SQL
     ⍝ command (i.e. it uses the UNION ALL operator).  This may be changed to use the UNION
     ⍝ operator instead to remove duplicate rows by specifying a Get-specific command option '∪'
     ⍝ (mnemonic for "unique") in the left argument.  Be advised that this duplicate-removal
     ⍝ request may require additional processing time for the database to perform.  All field names
     ⍝ are determined by those used in the first Select command and any Order By should use those
     ⍝ names (or ordinal numbers).
     ⍝
     ⍝ Another Get-specific command option is '?'.  A command option word may consist of one or
     ⍝ more '?' characters (e.g. '???') to indicate that those characters are to be prepended to
     ⍝ the (front of the) resulting SQL command.  This is merely a convenience option to avoid
     ⍝ having to explicitly catenate them for interfaces that use them.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Modified 2 December 2013 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 'Length Error'⎕SIGNAL 5/⍨(2≤|≡clauses)∧1>¯1↑1,⍴clauses
 :If 0=⎕NC'options' ⋄ options←0⍴⊂'' ⋄ :EndIf
 :If 1=|≡options ⋄ options←⊂options ⋄ :EndIf
 :If unionall←×≢options←(,,¨options)~⊂''
     unionall←~∨/t←∧/¨'∪'=(1⌈⍴¨options)↑¨options ⋄ options←(~t)/options
 :EndIf
 :If quest←×≢options
     quest←⌈/0,t←+/¨'?'=(1⌈⍴¨options)↑¨options ⋄ options←(t=0)/options
 :EndIf
     
 :If 1≥⍴⍴clauses ⍝ Generate a simple Select statement
     :If 1≥|≡clauses ⋄ clauses←,⊂,clauses ⋄ :EndIf
     select from where order group having←6↑clauses,6⍴⊂''
     :Trap 0
         :If 0∊⍴from
             sql←options Select select
         :Else
             sql←(options Select select),(From from),(Where where),(OrderBy order),(GroupBy group),Having having
         :EndIf
     :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
 :Else ⍝ Generate a Select/Union statement
     'Length Error (Union)'⎕SIGNAL 5/⍨4<¯1↑⍴clauses ⋄ order←0⍴⊂''
     :If 4≤¯1↑⍴clauses ⋄ :AndIf 1<⍴t←order←clauses[;4] ⋄ :AndIf 1<⍴t←order←(×≢¨∆dim¨t)/t
         'Domain Error (Order By)'⎕SIGNAL 11/⍨~∧/2≡/t
     :EndIf
     order←⊃order ⋄ sql←''
     :For clauses :In ↓clauses
         select from where←3↑clauses,3⍴⊂''
         :If ×⍴sql ⋄ sql,←' Union ',unionall/'All ' ⋄ :EndIf
         :If 0∊⍴from
             t←~(∆uc⊃¨∆cut¨options)∊∆uc'Distinct' 'Top' ⍝ Option keywords to ignore when selecting constants
             :Trap 0 ⋄ sql,←(t/options)Select select ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
         :Else
             :Trap 0 ⋄ sql,←(options Select select),(From from),Where where ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
         :EndIf
     :EndFor
     :Trap 0 ⋄ sql,←OrderBy order ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
 :EndIf
 sql←(quest⍴'?'),sql ⍝ Allow special prefix code of one or more "?"s
∇

∇ sql←GroupBy fields
     ⍝∇ Produce a SQL "Group By" clause using the given field names
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡fields ⋄ fields←⊂,fields ⋄ :EndIf
 sql←', '∆sew'.'Q fields ⋄ sql←((×⍴sql)/' Group By '),sql
∇

∇ sql←Having phrases;t
     ⍝∇ Generate a SQL "Having" clause from the argument(s) given.
     ⍝ This operates just like a "Where" clause, except none of the expressions
     ⍝ will be quoted as a field (since they should ALL be calculated values).
     ⍝ One or more phrases may be specified in any of the following formats:
     ⍝   (1) A complete "Having" clause (except for the text 'Having' itself)
     ⍝   (2) One or more phrases to be ANDed together to form the final clause
     ⍝   (3) Any phrase (above) may instead be specified as a nested formula-
     ⍝       value pair to be given to Is before being further processed.
     ⍝   (4) A two-column matrix of formula-value pairs to be passed to Is.
     ⍝ An empty call will produce a '' result (no "Having" keyword)
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡phrases ⋄ phrases←,⊂phrases ⋄ :EndIf
 :If 2=⍴⍴phrases ⋄ phrases←↓phrases ⋄ :EndIf ⋄ phrases←,,¨phrases
 :If ∨/t←6=10|⎕DR¨phrases ⍝ Run name/value pairs through Is
     'Length Error'⎕SIGNAL 5/⍨∨/2≠≢¨t/phrases
     (t/phrases)←⊃¨Is/¨(⊂'⍎' ''),¨¨t/phrases ⍝ (Unquotable)
 :EndIf
 sql←And ∆dlt¨phrases ⋄ sql←((×⍴sql)/' Having '),sql
     
     ⍝? Allow use of deeply-nested data to call Math internally?
∇

∇ sql←Ins clauses;table;values;fields
     ⍝∇ Generate a SQL Insert command from its component clauses, by calling
     ⍝∇ Insert and Values and catenating the results together.
     ⍝
     ⍝ Provide arguments in this order (the standard order of a SQL Insert statement):
     ⍝       {insert-table} {values}
     ⍝ {values} should be a simple or nested vector of data items to insert into
     ⍝ one record.
     ⍝
     ⍝ A specific field list, if used, may be specified in either argument using
     ⍝ one of the following choices:
     ⍝   {insert-table} may be a nested two-item vector containing:
     ⍝       {tablename} {fieldnames}
     ⍝ or
     ⍝   {values} may include both field names and values as a two-column matrix:
     ⍝       {fieldnames} ,[1.5] {values}
     ⍝
     ⍝ Written 17 December 2014 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 'Length Error'⎕SIGNAL 5/⍨(2≤|≡clauses)∧2≠¯1↑1,⍴clauses
 table values←clauses ⋄ fields←⍬
 :If 1<|≡table
     'Length Error'⎕SIGNAL 5/⍨2≠⍴table
     table fields←table
 :ElseIf 2=⍴⍴values
     'Length Error'⎕SIGNAL 5/⍨2≠1↓⍴values
     fields values←↓[1]values
 :ElseIf 2<|≡values
     'Length Error'⎕SIGNAL 5/⍨∨/2≠≢¨values
     fields←⊃¨values ⋄ values←2⊃¨values
 :EndIf
 'Rank Error'⎕SIGNAL 4/⍨1≠⍴⍴values
 :Trap 0
     :If 0∊⍴fields ⋄ sql←Insert table ⋄ :Else ⋄ sql←fields Insert table ⋄ :EndIf
     sql,←Values values
 :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
∇

∇ sql←{fields}Insert table
     ⍝∇ Produce a SQL INSERT clause with single table name & optional field names.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡table ⋄ :If 2=⍴⍴table ⋄ table←↓table ⋄ :Else ⋄ table←⊂table ⋄ :EndIf ⋄ :EndIf
 table←(∆dlt¨,,¨table)~⊂''
 'Missing table name to insert into'⎕SIGNAL 11/⍨0∊⍴table
 'Insert cannot be done into multiple tables'⎕SIGNAL 11/⍨1<≢table
 'Insert cannot be done into multiple tables'⎕SIGNAL 11/⍨∨/'→><≠∘='∊⊃table
 'Joined fields not permitted in a single table'⎕SIGNAL 11/⍨∨/'(,)'∊⊃table
 :Trap 0 ⋄ sql←'Insert Into',5↓From table ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
 :If 2=⎕NC'fields'
     :If 2=⍴⍴fields ⋄ fields←↓fields ⋄ :EndIf
     :If 1≥|≡fields ⋄ fields←⊂fields ⋄ :EndIf ⋄ fields←,,¨fields
     sql,←' ','()'∆Q', '∆sew Q fields
 :EndIf
∇

∇ sql←Into table
     ⍝∇ Used for SELECT INTO phrasing (must be into a single table)
     ⍝∇ Not used for INSERT INTO, as that is handled by Insert.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡table ⋄ :If 2=⍴⍴table ⋄ table←↓table ⋄ :Else ⋄ table←⊂table ⋄ :EndIf ⋄ :EndIf
 table←(∆dlt¨,,¨table)~⊂''
 'Missing table name to select into'⎕SIGNAL 11/⍨0∊⍴table
 'Into cannot be done into multiple tables'⎕SIGNAL 11/⍨1<≢table
 'Into cannot be done into multiple tables'⎕SIGNAL 11/⍨∨/'→><≠∘='∊⊃table
 'Joined fields not permitted in a single table'⎕SIGNAL 11/⍨∨/'(,)'∊⊃table
 :Trap 0 ⋄ sql←' Into ',6↓From table ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
∇

∇ sql←fieldname Is starpattern;t;pat;origfield;q;dr;num;op;sop;upper;wild;asis;likeonly;nolike;⎕PP;math;zilde;tcnul
     ⍝∇ Change a (possibly "wildcarded") search request to a SQL "WHERE" phrase
     ⍝∇ using '=' (equals), 'IN', 'LIKE', or 'BETWEEN', or 'NOT' versions of
     ⍝∇ these, or inequalities ('<', '<=', '>=', '>').  Also supports testing
     ⍝∇ of 'NULL' values, case-insensitive text and IN/EXISTS subqueries.
     ⍝
     ⍝ --- Left argument:
     ⍝ The name of the SQL field to be compared, or multiple fields to compare
     ⍝ simultaneously as if an APL "each" ("¨") were used, then ANDed together.
     ⍝ (Further syntax descriptions below assume a single field name is used.)
     ⍝ If the field name contains a "~" character, the test is to be "notted",
     ⍝ i.e. = becomes <>, LIKE becomes NOT LIKE, IN becomes NOT IN, etc.
     ⍝ If the field name contains a "↑" character, this is a shorthand notation
     ⍝ for an upper-case-only-text comparison.
     ⍝ The field name may also contain one of the following APL symbols
     ⍝ imbedded in the name to indicate that the comparison is not a simple
     ⍝ equality, as follows:
     ⍝   =   Equals or Like or In (default)
     ⍝   ≠   Not equals or Not Like or Not In (default if "~" is used)
     ⍝   ....The following choices do not allow wildcards or lists:
     ⍝   <   Field less than value
     ⍝   ≤   Field less than or equal to value
     ⍝   ≥   Field greater than or equal to value
     ⍝   >   Field greater than value
     ⍝   →   Field between two values (two nested values must be supplied)
     ⍝   ∊   Field is IN subquery or subquery result EXISTS
     ⍝ Sometimes, a SQL field needs to be compared to another field or formula,
     ⍝ rather than a constant value.  To indicate that Is should compare to a
     ⍝ literal (unprocessed) character string, include the APL Quote-Quad ("⍞")
     ⍝ symbol as part of the field name.  The data argument should then be the
     ⍝ text that you wish to appear exactly as-is in the SQL command (without
     ⍝ any quoting or other processing beyond space-trimming).
     ⍝ Alternatively, include the APL Divide ("÷") symbol in the name and make
     ⍝ the right argument a nested vector.  When this is used, Math is called
     ⍝ and the first item of the nested right argument is passed as the Math
     ⍝ left argument and the remainder of the nested right argument is passed
     ⍝ as the Math right argument.  The result is then used as an as-is data
     ⍝ value, as if the field name had contained "⍞".
     ⍝ Sometimes, especially in conjunction with the "⍞" option above, it may
     ⍝ be desirable to limit the operation to a "LIKE" comparison only.  This
     ⍝ may be accomplished by including a "*" character in the field name, and
     ⍝ it may only be used with the "=" or "≠" comparison operations.  This
     ⍝ allows (usually with "⍞") the generation of LIKE-comparisons with
     ⍝ substitution parameters, or (without "⍞") providing values that have
     ⍝ already been pre-formatted for use with the SQL LIKE operator (no
     ⍝ further wildcard conversion or reserved-character escaping is done).
     ⍝ Is can be called as a utility to perform LIKE-compatible wildcard
     ⍝ conversion with reserved-character escaping.  Call it with the data as
     ⍝ the right argument and a '*' character as the left argument (no field
     ⍝ name at all) - this invokes the special "likeness" encoding feature.
     ⍝ Sometimes a value needs to be specified that contains the "*" character,
     ⍝ but it is to be treated as an actual matching character rather than a
     ⍝ "wildcard".  To use a non-wildcarded test, include the "⍟" character as
     ⍝ part of the field name.
     ⍝ The "∊" comparison is used to generate a subquery.  When used, the right
     ⍝ argument should be a SQL Select command, already properly formed but
     ⍝ without parentheses.  (Optionally, the right argument could be a nested
     ⍝ structure suitable for passing to Get.)  The right argument will be
     ⍝ treated as a literal string (as in "⍞") rather than a data value.  The
     ⍝ field name will be checked to see if it's "IN" the result of the
     ⍝ subquery.  If no field name is supplied (an exception to the general
     ⍝ rule), then the subquery will be checked for existence of any result
     ⍝ with the "EXISTS" keyword.  This comparison can be combined with the "~"
     ⍝ option to generate "NOT IN" or "NOT EXISTS" instead.  Be cautious of
     ⍝ using the "↑" option here, as the subquery will need to return only
     ⍝ upper-case values.
     ⍝ Finally, the left argument sometimes (such as in a "HAVING" clause)
     ⍝ needs to be a calculation instead of a field name.  In such a case
     ⍝ include the APL execute ("⍎") symbol as part of the name and the rest
     ⍝ of the name (with APL symbols removed) will be used as a formula and
     ⍝ will not be named-quoted if it contains non-standard symbols (which it
     ⍝ almost certainly will).  However, use of this option produces a side-
     ⍝ effect ... many formulas may contain some special characters that the
     ⍝ Is function is treating specially.  Therefore, if you use the "⍎"
     ⍝ symbol in the left argument, then the following symbols are treated as
     ⍝ part of the SQL phrase instead of being treated as special commands:
     ⍝ "=", "<", ">", and "*".  To reiterate, you may not use any of those four
     ⍝ special comparison operations at the same time as you use "⍎" (they will
     ⍝ be treated as part of the SQL computation instead).
     ⍝ Alternatively, instead of using "⍎" and pre-constructing the expression,
     ⍝ you may instead nest the calculation, leave "⎕" characters in place of
     ⍝ field names and other values, and append the contents of those values to
     ⍝ the nested expression.  This will cause Math to be called with the
     ⍝ expression definition (first item) as its left argument and the rest of
     ⍝ the nested vector as its right argument.  The result from Math is then
     ⍝ used as an as-is field name as if "⍎" was specified.
     ⍝ So the non-comparison-operator options are:
     ⍝   ~   Invert ("not") the named or assumed comparison test
     ⍝   ↑   Perform upper-case comparisons
     ⍝   ⍬   Allow empty vectors and all-space character values to be matched
     ⍝   ⍞   Literal as-is value specification (for formulas, etc.)
     ⍝   *   Force the use of the LIKE operator (no inequalities)
     ⍝   *   Without a field name, just LIKE-encode the right argument
     ⍝   ⍟   Prevent use of the LIKE operator
     ⍝   ⍎   Use the field name as an un-quoted computation expression
     ⍝   ÷   Invoke Math to construct an computation expression for the data
     ⍝ If all this is too complicated for you, just stick with field names and,
     ⍝ optionally, the "~".  That'll cover most needs and anything special you
     ⍝ can just type in your own expression without using Is.
     ⍝ If you find that you need anything more complicated than Is can handle,
     ⍝ which isn't too likely, then do that without Is as well.
     ⍝
     ⍝ --- Right argument:
     ⍝ The data value(s) to compare to the above-named field.
     ⍝ Trailing spaces are trimmed from the value(s) before processing.
     ⍝ The incoming wildcard character is usually '*'.
     ⍝ '' and ⍬ (empty vector) and single all-blank values are ignored
     ⍝ entirely and produce no SQL restriction at all, unless the '⍬' option
     ⍝ is used to override this behavior.
     ⍝ A data pattern composed only of wildcard characters ('*') is ignored
     ⍝ for equals-matching purposes and also produces no restriction (as would
     ⍝ be expected).  A not-equals test, however, produces a restriction that
     ⍝ will NEVER match any data (for consistency).
     ⍝ If the right argument is nested or a numeric vector, it may produce an
     ⍝ "IN" phrase or a series of "OR"ed phrases instead of an '='.
     ⍝ Any unnested data that is all blanks (and thus an empty '' after
     ⍝ trimming) does not normally produce any restriction (as noted above).
     ⍝ But an all-blank item (not a '' before trimming) within a nested list of
     ⍝ items is preserved and is always considered a valid choice of value to
     ⍝ match, even if the '⍬' override is not specified.
     ⍝ So if you need to Is to a real blank (without using the '⍬' code), or
     ⍝ to any vector of scalar character values, they need to be provided as a
     ⍝ nested vector. The easiest way to do this is to use ravel-each (",¨") on
     ⍝ the choices first (e.g. ,¨' ' or ,¨'ABC').
     ⍝ To reiterate:  Without the '⍬' override code, '' is always ignored and
     ⍝ all-' ' data is only used if it's nested.
     ⍝ A data value of ⎕UCS 0 (ASCII NUL character) or the ⎕NULL value can
     ⍝ be used to produce an "IS NULL" test.
     ⍝ If the "÷" option is invoked, then the right argument should be a nested
     ⍝ vector, the first item of which is used as the left argument to Math
     ⍝ and the remainder of the vector is provided as the right argument to
     ⍝ Math.
     ⍝
     ⍝ --- Matrix right argument:
     ⍝ It is sometimes desired to produce a series of multi-field conditions
     ⍝ (each of which is ANDed together as usual) where any of the multi-field
     ⍝ conditions may be matched.  In other words, several (parenthesized)
     ⍝ expressions need to be ORed together, each of which is a set of the
     ⍝ same fields being checked and ANDed together.  For instance:
     ⍝   (A = 1 And B = 1) Or (A = 2 And B = 2) Or (A = 3 And B = 3)
     ⍝ Of course, this can be done by using Is¨ and Or-ing the result, using
     ⍝ parentheses appropriately, but this can be a little awkward to code.
     ⍝ To ask Is for this multi-layer comparison, pass it a single list of
     ⍝ field names as a left argument (A & B in this case) and pass it a matrix
     ⍝ of values to match in the right argument (3 2⍴1 1 2 2 3 3 in this case).
     ⍝ Is will perform it's normal construction work on each row of the matrix
     ⍝ (reusing the field list for each row) and it will then OR the rows
     ⍝ together for a final result, with parentheses as needed.
     ⍝ Some extraneous parentheses might occasionally appear if Is cannot
     ⍝ ensure safe imbedding in a longer SQL clause.
     ⍝
     ⍝ --- Examples:
     ⍝       'davin' Is 'Done'
     ⍝   davin = 'Done'
     ⍝       'davin~' Is 'Done'
     ⍝   davin <> 'Done'
     ⍝       'davin↑' Is 'Done'
     ⍝   Upper(davin) = 'DONE'
     ⍝       'davin' Is 10×⍳5
     ⍝   davin In (10, 20, 30, 40, 50)
     ⍝       'davin' 'church' Is 5 10
     ⍝   davin = 5 And church = 10
     ⍝       'davin' Is '*hungry*'
     ⍝   davin Like '%hungry%'
     ⍝       'davin∊' Is 'Select FirstName From AddressBook'
     ⍝   davin In (Select FirstName From AddressBook)
     ⍝       'davin' 'doug' Is 2 2⍴⍳4
     ⍝   ((davin = 1 And doug = 2) Or (davin = 3 And doug = 4))
     ⍝       'davin' Is ''
     ⍝
     ⍝       'davin' Is ,¨' '
     ⍝   davin = ''
     ⍝       'davin⍬' Is ''
     ⍝   davin = ''
     ⍝       'davin' Is ,¨'ABC'
     ⍝   davin In ('A', 'B', 'C')
     ⍝
     ⍝
     ⍝ Subroutines required:  And, Null, Or, Q, ∆Q, ∆csv, ∆dtr
     ⍝ Subroutines sometimes needed:  Get (w/subroutines), Math (w/subroutines), ∆dlt, ∆sew, ∆uc
     ⍝
     ⍝ Written about 1999 by Davin Church of Creative Software Design
     ⍝ Modified 5 January 2018 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 ⎕PP←15 ⋄ q wild tcnul←'''*',⎕UCS 0 ⋄ :If 0<⎕NC'Config' ⋄ q wild←Config[4 5] ⋄ :EndIf
 :If 2=⍴⍴starpattern ⍝ A matrix of values requests And/Or alternative grouping
     sql←(⊂fieldname)Is¨↓starpattern←' '∆dtr¨starpattern
     sql←((1<t←≢¨(↓starpattern)~¨⊂''⍬)⍴¨⊂⊂'()')And¨sql
     sql←((1<+/t)/'()')Or sql
     :Return
 :EndIf
 :While ∨/math←1<|≡¨fieldname ⍝ If it's nested more than 1 deep, it must be a Math call
     :If 0∊⍴⍴math ⋄ fieldname←⊃fieldname ⋄ :EndIf ⍝ Allow for nested-scalar processing
     'Depth error on field expression'⎕SIGNAL 11/⍨1≠|≡⊃pat←(math←(,math)⍳⍳⍴⍴math)⊃fieldname
     :Trap 0 ⋄ pat←(⊃pat)Math 1↓pat ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap ⍝ Perform Math substitution
     (math⊃fieldname)←pat,'⍎'~pat
 :EndWhile ⍝ They better all be unnested names now
 :If 1<|≡fieldname ⍝ Multiple field names indicate multiple "likes" to be And-ed together
     'Length Error'⎕SIGNAL 5/⍨(⍴1/fieldname)≢⍴1/starpattern
     :Trap 0 ⋄ sql←And fieldname Is¨starpattern ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap ⋄ :Return
 :EndIf
 t←'↑⍬⍞*⍟⍎÷~=≠<≤≥>→←∊⍷' ⋄ :If '⍎'∊fieldname ⋄ t←t~'=<>*' ⋄ :EndIf ⍝ Options
 op←(t∊fieldname)/t ⋄ op←op,(0∊⍴op~'↑⍬⍞*⍟⍎~÷')/'='
 origfield←fieldname ⋄ fieldname←∆dlt fieldname~t
 :If (×⍴fieldname)∧~'⍎'∊op ⋄ fieldname←'.'Q fieldname ⋄ :EndIf
 'Conflicting comparison operations'⎕SIGNAL 11/⍨1≠≢op~'↑⍬⍞*⍟⍎~÷'
 'Conflicting "Like" ("*"/"⍟") operations'⎕SIGNAL 11/⍨∧/'*⍟'∊op
 :If upper←'↑'∊op ⋄ fieldname←'Upper(',fieldname,')' ⋄ :EndIf
 zilde asis likeonly nolike math←'⍬⍞*⍟÷'∊op ⋄ op←op~'↑a⍞*⍟⍎÷' ⍝ Flags
 :If '~'∊op ⋄ op←'≠=≥><≤←→⍷∊'['=≠<≤≥>→←∊⍷'⍳op~'~'] ⋄ :EndIf ⍝ Invert test
 sop←'=' '<>' '<' '<=' '>=' '>' 'Between' 'Not Between'
 sop,←(1+0∊⍴fieldname)⊃('In' 'Not In')('Exists' 'Not Exists')
 sop←(0∊⍴fieldname)↓' ',(('=≠<≤≥>→←∊⍷'⍳op←⊃op)⊃sop),' '
 'Inequalities cannot be used with "Like" ("*")'⎕SIGNAL 11/⍨likeonly∧~op∊'=≠'
 num←1 3 5 7∊⍨dr←10|⎕DR starpattern←tcnul Null starpattern ⍝ What data type is this?
 '"Uppercase" ("↑") cannot be applied to numeric values'⎕SIGNAL 11/⍨num∧upper
 :If op∊'∊⍷' ⍝ '⍷' means not-in/not-exists internally
     :If dr=6
         :Trap 0 ⋄ dr←10|⎕DR starpattern←Get starpattern ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
         num←0
     :EndIf
     starpattern←'()'∆Q starpattern ⋄ asis←1
 :EndIf
 :If math
     :If 0∊⍴starpattern ⍝ Empty Math expression should be ignored as a ''
         dr←10|⎕DR starpattern←'' ⋄ num←0 ⋄ asis←0
     :Else
         'Depth error on Math value expression'⎕SIGNAL 11/⍨1≠|≡⊃starpattern
         :Trap 0 ⋄ dr←10|⎕DR starpattern←(⊃starpattern)Math 1↓starpattern ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
         num←0 ⋄ asis←1
     :EndIf
 :EndIf
 :If asis ⍝ Just plug the text straight into the SQL command
     '"Null" cannot be used in as-is tests'⎕SIGNAL 11/⍨tcnul∊∊starpattern
     :If likeonly ⋄ sop←((op='≠')/' Not'),' Like ' ⋄ :EndIf
     :If op∊'→←' ⍝ Between two values
         :If (2≠≢starpattern)∨(~zilde)∧∨/0∊¨⍴¨starpattern
         :OrIf (≢/⊃¨0⍴¨starpattern)∨∨/(0≠⊃⊃0⍴⊃starpattern)<|≡¨starpattern
         :OrIf ∨/tcnul∊¨starpattern
             '"Between" ("→") must use exactly two single values of the same type'⎕SIGNAL 11
         :EndIf
         sql←fieldname,sop,(⍕1⊃starpattern),' And ',⍕2⊃starpattern
     :ElseIf dr=6
         :If (op∊'=≠')∧(~likeonly)∧1<⍴starpattern~←(~zilde)/''⍬
             sql←fieldname,((op='≠')/' Not'),' In ','()'∆Q'' ', '∆csv starpattern
         :Else
             sql←(×⍴starpattern)/((1<⍴,starpattern)/'()')∆Q Or(⊂fieldname,sop),¨⍕¨starpattern
         :EndIf
     :Else
         sql←(×⍴starpattern~(~zilde)/''⍬)/fieldname,sop,⍕starpattern
     :EndIf
 :ElseIf dr=6
 :OrIf num∧1<⍴,starpattern
         ⍝ Nested calls generate an IN-list and/or a OR-list of "likes"
     :If ×⍴starpattern~←(~zilde)/''⍬
         :If ((wild∊∊starpattern)∨∨/tcnul∊¨starpattern)∧~nolike ⍝ Any wildcards (or nulls) at all?
         :OrIf 1=⍴starpattern ⍝ (a single term produces simple result)
             sql←'' ⋄ t←(likeonly∨1∊⍴starpattern)∨(('*'∊¨starpattern)∧~nolike)∨tcnul∊¨starpattern
             :If ∨/~t ⋄ :Trap 0 ⋄ sql,←⊂origfield Is(~t)/starpattern ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap ⋄ :EndIf
             :If ∨/t ⋄ :Trap 0 ⋄ sql,←(⊂origfield)Is¨t/starpattern ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap ⋄ :EndIf
             :If ∨/t←t∧1≥|≡¨starpattern
             :AndIf ∨/t←t\∧/¨t/starpattern=' ' ⍝ Preserve "empty" checks in lists
                 'Inequalities cannot be used with wildcards'⎕SIGNAL 11/⍨~op∊'=≠'
                 (t/sql)←⊂fieldname,sop,q ∆Q''
             :EndIf
             :If (∨/0∊¨⍴¨sql)∨(∨/∧/¨starpattern=wild)∧~nolike
                     ⍝ This is an "*" (or blank) condition somewhere in the list.
                     ⍝ If it's an "=" choice, then it will select all records.
                     ⍝ If it's a "≠" choice, then it will select NO record!
                     ⍝ We'll simplify both these situations into a single true/false
                 'Inequalities cannot be used with wildcards'⎕SIGNAL 11/⍨~op∊'=≠'
                 sql←⊂(op='≠')/fieldname,' Not Like ',q ∆Q'%'
             :EndIf
             sql~←⊂''
             :If op='≠' ⋄ sql←And sql ⋄ :Else ⋄ sql←((1<⍴,sql)/'()')∆Q Or sql ⋄ :EndIf
         :Else ⍝ Nope, then try using 'IN' instead
             t←0 2∊⍨10|⎕DR¨starpattern ⋄ (t/starpattern)←∆dtr¨t/starpattern ⍝ Trim
             :If upper ⋄ (t/starpattern)←∆uc t/starpattern ⋄ :EndIf ⍝ Uppercase
             :If likeonly
                 :Trap 0 ⋄ sql←((⊂origfield)Is¨starpattern)~⊂'' ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
                 :If op='≠' ⋄ sql←And sql ⋄ :Else ⋄ sql←((1<⍴,sql)/'()')∆Q Or sql ⋄ :EndIf
             :ElseIf op∊'→←' ⍝ Between two values
                 :If (2≠≢starpattern)∨(~zilde)∧∨/0∊¨⍴¨starpattern
                 :OrIf (≢/⊃¨0⍴¨starpattern)∨∨/(0≠⊃⊃0⍴⊃starpattern)<|≡¨starpattern
                 :OrIf ∨/tcnul∊¨starpattern
                     '"Between" ("→") must use exactly two single values of the same type'⎕SIGNAL 11
                 :EndIf
                 sql←fieldname,sop,(q ∆csv starpattern[1]),' And ',q ∆csv starpattern[2]
             :Else
                 'Inequalities cannot be used with lists'⎕SIGNAL 11/⍨~op∊'=≠'
                 sql←fieldname,((op='≠')/' Not'),' In ','()'∆Q q', '∆csv starpattern
             :EndIf
         :EndIf
     :Else
         sql←'' ⍝ Nope, nothing here to restrict on
     :EndIf
 :ElseIf likeonly ⍝ Is a pre-formatted LIKE requested?
     '"Like" ("*") cannot be used with numeric values'⎕SIGNAL 11/⍨num
     '"Like" ("*") cannot be used for "Null" tests'⎕SIGNAL 11/⍨tcnul∊∊starpattern
     pat←∆dtr starpattern ⋄ :If upper ⋄ pat←∆uc pat ⋄ :EndIf ⍝ Uppercase?
     :If ×⍴fieldname
         sql←(×⍴pat)/fieldname,((op='≠')/' Not'),' Like ',q ∆Q pat
     :Else ⍝ They're asking for a "likeness" to be performed instead of creating a SQL command
         :If ∧/pat∊wild ⋄ pat←⊃wild ⋄ :EndIf
         t←,pat∊'%_\'~wild ⋄ pat←(1+t)/pat ⋄ pat[(t/⍳⍴t)+¯1+⍳+/t]←'\' ⍝ Characters to 'escape' in string first
         sql←('%'@{⍵∊wild})pat ⍝ Change all '*'s to '%'s
     :EndIf
 :ElseIf (~zilde)∧(~num)∧0∊⍴pat←∆dtr starpattern ⍝ If a '' after trimming
 :OrIf (×⍴pat)∧(~num)∧(∧/pat∊wild)∧~nolike ⍝ Or if all '*' characters
 :OrIf (~zilde)∧num∧0∊⍴starpattern ⍝ Or if a numeric ⍬ (before trimming)
         ⍝ This is an "*" (or blank) condition.
         ⍝ If it's an "=" choice, then it will select all records.
         ⍝ If it's a "≠" choice (and not a ''), then it will select NO record!
         ⍝ We'll simplify both these situations into a single true/false
     'Inequalities cannot be used with wildcards'⎕SIGNAL 11/⍨(wild∊pat)∧~op∊'=≠'
     sql←((op='≠')∧(~num)∧×⍴pat)/fieldname,' Not Like ',q ∆Q'%'
 :ElseIf (wild∊pat)∧~nolike ⍝ Anything wild?
     'Inequalities cannot be used with wildcards'⎕SIGNAL 11/⍨~op∊'=≠'
     :If upper ⋄ pat←∆uc pat ⋄ :EndIf ⍝ Uppercase
     sql←fieldname,((op='≠')/' Not'),' Like ',q ∆Q'*'Is pat ⍝ Likeness-it before composing SQL
 :Else ⍝ ** Exact match
     '"Between" ("→") must use exactly two single values of the same type'⎕SIGNAL 11/⍨op∊'→←'
     :If (×⍴,starpattern)∧∧/starpattern∊⎕UCS 0
         'Inequalities cannot be used for "Null" tests'⎕SIGNAL 11/⍨~op∊'=≠'
         sql←fieldname,' Is',((op='≠')/' Not'),' Null'
     :Else
         :If 0 2∊⍨10|⎕DR starpattern
             starpattern←∆dtr starpattern ⍝ Trim
             :If upper ⋄ starpattern←∆uc starpattern ⋄ :EndIf ⍝ Uppercase
         :Else
             starpattern←⊃starpattern
         :EndIf
         sql←fieldname,sop,q ∆csv⊂starpattern
     :EndIf
 :EndIf
∇

∇ sql←expression Math data;t;⎕PP
     ⍝∇ Generate the SQL text to call SQL functions and perform calculations involving field names
     ⍝∇ that may need to use the Q utility on the field names before imbedding them in the formulas,
     ⍝∇ but without having to deal with many confusing parentheses, commas, and quotes trying to
     ⍝∇ create such expressions in direct code.
     ⍝
     ⍝ The left argument "expression" is the text of the calculation to be performed (by SQL),
     ⍝ except with the character "⎕" used in place of any field name (or other value to be
     ⍝ substituted into the expression).  Each "⎕" is replaced with the value from the next item
     ⍝ of the right argument.  Be sure to code parentheses ("()") and commas (",") appropriately
     ⍝ for any SQL function calls.
     ⍝
     ⍝ For easier use, when calling Math to format a simple call to a single function and only
     ⍝ want the entire argument list to be provided that function call, then the left argument
     ⍝ may be shortened to just the name of the function to be called, without any parentheses,
     ⍝ commas, or other operations.  The arguments will then be appended to that function name,
     ⍝ surrounded by parentheses, to invoke a call to that SQL function.
     ⍝
     ⍝ The right argument "data" should be one or more values to be provided as substitutions
     ⍝ into the expression.  If it is a single value, then a number or a text field name may
     ⍝ be passed as an unnested scalar or vector.  Otherwise the right argument should be a
     ⍝ (usually nested) vector of values, each of item of which should itself be in one of the
     ⍝ following structures:
     ⍝   * A single numeric value, to be used as a constant number in the expression.
     ⍝   * A simple (unnested) text string, to be used as a SQL field name (processed with Q).
     ⍝   * A singly-nested text string, to be used as a text constant (enclosed in quotes).
     ⍝   * A double-nested text string is a pre-formatted string to be inserted exactly as-is.
     ⍝ (Since the primary purpose of this utility is to pass a single SQL field name to a single
     ⍝ SQL function call, that syntax is designed to be the easiest to use -- see the first example.)
     ⍝
     ⍝ Since the purpose of this utility will often be to provide its result to companion utilities
     ⍝ such as Select, Where, or Is, if any of the special Is function codes are included in
     ⍝ the "expression" (left) argument, then they will remain in the output as a convenience aid
     ⍝ for passing on to the companion utility.
     ⍝
     ⍝ Is may also be given special codes and data structures to request that it call Math
     ⍝ internally.  (See Is for details.)
     ⍝
     ⍝ Examples:
     ⍝          'RTrim' Math 'field'
     ⍝    RTrim(field)
     ⍝          'RTrim' Math 'table.field'
     ⍝    RTrim("table".field)
     ⍝          'RTrim' Math 'funny%field'
     ⍝    RTrim("funny%field")
     ⍝          'Left' Math 'field' 10
     ⍝    Left(field,10)
     ⍝          'Left(⎕,⎕)' Math 'field' 10
     ⍝    Left(field,10)
     ⍝          'Concat' Math 'city' 'state'
     ⍝    Concat(city,state)
     ⍝          'Concat(⎕,⎕)' Math 'city' 'state'
     ⍝    Concat(city,state)
     ⍝          'Concat(Concat(⎕,⎕),⎕)' Math 'city' 'state' 'zip'
     ⍝    Concat(Concat(city,state),zip)
     ⍝          'Concat(Concat(RTrim(⎕),⎕),RTrim(⎕))' Math 'city' 'state' 'zip'
     ⍝    Concat(Concat(RTrim(city),state),RTrim(zip))
     ⍝          'Left(⎕,Length(⎕)-1)' Math 'field' 'field'
     ⍝    Left(field,Length(field)-1)
     ⍝
     ⍝ Subroutines required: Null, Q (w/ ∆dlt, ∆uc), ∆sew, ∆Q
     ⍝ Written 30 November 2015 by Davin Church of Creative Software Design
     ⍝ Modified 11 December 2015 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1 3 5 7 9∊⍨10|⎕DR data ⋄ data←,¨data ⋄ :EndIf ⍝ Make sure numbers are separated
 :If 1≥|≡data ⋄ data←⊂,data ⋄ :EndIf ⋄ data←,data ⍝ Nest single text arguments
     
 :If (×⍴expression)∧∧/expression∊'_',⎕D,⎕A,819⌶⎕A ⍝ They just provided a function name without any syntax
     expression←expression,'()'∆Q(¯1+2×⍴data)⍴'⎕,' ⍝ Assume they meant to add all these as parameters to a single fn call
 :EndIf
 ('Unequal number of parentheses in expression syntax: ',expression)⎕SIGNAL 11/⍨≠/+/'()'∘.=expression
 ('Mismatched parentheses in expression syntax: ',expression)⎕SIGNAL 11/⍨∨/0>-⌿+\'()'∘.=expression
 ('The number of ⎕ characters in expression (',(⍕+/'⎕'=expression),' does not match the total length of arguments (',(⍕≢data),')')⎕SIGNAL 1/⍨(≢data)≠+/'⎕'=expression
     
     ⍝ Encode values differently based on their data type & depth
 t←⎕DR data←((⊂⊂'Null()')@{⍵∊⎕UCS 0})data ⋄ t←((3⌊|≡¨data)@{6=10|⍵})t ⍝ Pre-figure depth of nested items
 ⎕PP←15 ⋄ t←({⍕¨⍵}@{(10|⍵)∊1 3 5 7 9})t ⍝ Simple numeric values are encoded as constant numbers
 t←({'.'Q¨⍵}@{(10|⍵)∊0 2})t ⍝ Simple text values are assumed to be [table.]field names (most common usage)
 t←({''''∆Q¨⊃¨⍵}@{⍵∊2})t ⍝ Singly-nested values are assumed to be text strings to be treated as constant text data
 t←({⊃¨⊃¨⍵}@{⍵∊3})t ⍝ Doubly-nested values are assumed to be text strings to be treated as pre-formatted arguments
 sql←⊃,/1↓,('⎕',⍕¨data),[1.5]1↓¨(+\'⎕'='⎕',expression)⊆'⎕',expression ⍝ Replace all the '⎕'s with massaged data
∇

∇ result←default Null data;rho;t
     ⍝∇ Replace any special ⎕NULL values in SQL data with values more easily handled by APL.
     ⍝
     ⍝ The right argument may be a single (simple, unnested) value or a nested array of values.
     ⍝ Any single value of the argument may instead be a special ("magic") ⎕NULL value.
     ⍝ Wherever encountered, this ⎕NULL value will be replaced by the left argument, usually
     ⍝ 0 or '' or similar, and the same structure (rank and rho) returned with those values
     ⍝ replaced.
     ⍝ Converted to Dyalog 12 December 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡data
          ⍝ In v17.1, "@" doesn't work properly on a simple scalar ⎕NULL value!
     :If ⎕NULL∊result←data ⋄ result←default ⋄ :EndIf
 :Else
     result←((⊂default)@{⍵∊⍬ 1⍴¨⎕NULL})data
 :EndIf
∇

∇ results←defaults Nulls matrix
     ⍝∇ Calls Null on a data vector/matrix with different defaults for each item/column.
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 'Rank Error: Only operates on vectors or matrices of data'⎕SIGNAL 11/⍨~(⍴⍴matrix)∊1 2
 'Rank Error: Default list must be a vector'⎕SIGNAL 11/⍨1≠⍴⍴defaults
 'Length Error: Length of defaults and data must match'⎕SIGNAL 11/⍨(⍴defaults)≠¯1↑⍴matrix
     ⍝ (Must {each} by column rather than looping through differing defaults, otherwise
     ⍝ the column datatypes may change due to demotion to a simple array.)
 :If 1=⍴⍴matrix
     results←defaults Null¨matrix
 :Else
     results←↑[0.5]defaults Null¨↓[1]matrix
 :EndIf
∇

∇ both←{left}Or right;pp;addp;cnt
     ⍝∇ Join two (or more) phrases together with a SQL 'Or' conjunction.
     ⍝ Handle cases where phrases may be empty.  (All-space phrases not supported.)
     ⍝ Allow argument(s) to be nested to specify multiple terms.
     ⍝ A special term of '()' causes non-empty results to be enclosed in parens.
     ⍝ A special term of '(,)' causes results to be enclosed in parens, but only if
     ⍝ there is more than one term being ORed together.  This assumes that the
     ⍝ arguments are individual terms that haven't already been combined.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Modified 14 November 2014 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 0=⎕NC'left' ⋄ left←'' ⋄ :EndIf ⋄ pp←'()' '(,)' ⋄ addp←0 0 ⋄ cnt←0 0
 :If 1<|≡left ⋄ addp∨←pp∊left ⋄ cnt[1]←⍴left←left~pp,⊂'' ⋄ left←⊃Or/(⊂''),left ⋄ :EndIf
 :If 1<|≡right ⋄ addp∨←pp∊right ⋄ cnt[2]←⍴right←right~pp,⊂'' ⋄ right←⊃Or/(⊂''),right ⋄ :EndIf
 addp∨←pp∊left right ⋄ left←(~(⊂left)∊pp)/left ⋄ right←(~(⊂right)∊pp)/right ⋄ cnt+←(cnt=0)××≢¨left right
 both←left,((∧/×(⍴left),⍴right)/' Or '),right
 :If ∨/addp∧(×⍴both),1<+/cnt ⋄ both←'()'∆Q both ⋄ :EndIf
∇

∇ sql←OrderBy fields;desc
     ⍝∇ Produce a SQL "Order By" clause using the given field names
     ⍝∇ Field names may contain ⍋ or ⍒ characters to indicate ascending (default) or descending sorts.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
     
 :If 1≥|≡fields ⋄ fields←⊂,fields ⋄ :EndIf
 desc←('' ' Desc')[1+'⍒'∊¨fields←,,¨fields] ⋄ fields~¨←⊂'⍋⍒'
 sql←', '∆sew('.'Q fields),¨desc ⋄ sql←((×⍴sql)/' Order By '),sql
∇

∇ newwords←{dot}Q words;t;simple;legal;x;w;q;rw;lc;ss
     ⍝∇ Enclose illegal or reserved word(s) in SQL escape-quote marks
     ⍝ Database configuration settings contained in global: Config
     ⍝ Optional left argument is a "." character.  If provided, cut words on dots and quote each subword.
     ⍝ Words containing '⍎' are never quoted and the '⍎' is removed.
     ⍝ Words nested an extra level are treated as formulas and never quoted.
     ⍝ Words that are exactly '*' or end in '(*)' are never quoted.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 q←'""' ⋄ lc←'_' ⋄ rw←ss←0⍴⊂'' ⋄ :If ×⎕NC'Config' ⋄ q lc ss rw←Config[3 2 6 9] ⋄ :EndIf
 :If simple←1≥|≡words ⋄ words←⊂,words ⋄ :EndIf ⋄ words←,¨words
 :If 0=⎕NC'dot' ⋄ dot←'' ⋄ :EndIf ⋄ legal←lc,⎕D,⎕A,819⌶⎕A
 :If ∨/~w←,1≥|≡¨words ⋄ ((~w)/,words)←⊃¨(~w)/,words ⋄ :EndIf
 newwords←∆dlt¨words ⍝ Don't allow leading or trailing spaces on names
 :For x :In (∨/w)/ss ⋄ (((∊w/,newwords)∊⊃x)/∊w/,newwords)←⊃⌽x ⋄ :EndFor
 :If ∨/t←w∧,(1∊¨newwords∊¨⊂dot)∧~'⍎'∊¨newwords
     x←(⊃dot),¨t/,newwords ⋄ x←(x∊¨⊂dot)⊂¨x
     (t/,newwords)←1↓¨∆sew¨(1↑¨¨x),¨¨Q¨1↓¨¨x
 :EndIf
     
 w∧←(~t)∧,((∆uc¨newwords)∊rw)∨(~1↓[1]∧/¨1⍪newwords∊¨⊂legal)∨(⊃¨newwords)∊¨⊂(10+⍴lc)↑legal
 w∧←~,(newwords∊⊂,'*')∨(¯3↑¨newwords)∊⊂'(*)' ⍝ Preserve special cases as always-keyword
 :If ∨/t←'⍎'∊¨w/,newwords ⋄ (t/w/,newwords)←(t/w/,newwords)~¨'⍎' ⋄ w←w\~t ⋄ :EndIf
 :If ∨/w ⋄ w←w\(⊂⌽2⍴q)≢¨¯1⌽¨w/,newwords ⋄ :EndIf ⍝ Ignore already-quoted words
 :If ∨/w ⋄ (w/,newwords)←q ∆Q w/,newwords ⋄ :EndIf ⍝ Quote anything that's needed
 :If simple ⋄ newwords←⊃newwords ⋄ :EndIf
     
     ⍝? Allow a left argument flag of '!' or similar to indicate that it should always be force-quoted?  Don't need?
∇

∇ mat←{pw}See sql;kw;ind;mark;this;at;phrase;m;t;i;nl
     ⍝∇ Format a SQL command as a prettily-wrapped matrix
     ⍝
     ⍝ Written about 2005 by Davin Church of Creative Software Design
     ⍝ Modified 16 December 2015 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 0=⎕NC'pw' ⋄ pw←⎕PW ⋄ :EndIf ⋄ nl←⎕UCS 13
 ind←1↑1↓pw,4 ⋄ pw←⊃pw ⋄ mat←'' ⋄ sql←∆dmu,' ',(' '@{⍵∊nl})sql
 'Minimum display width exceeded'⎕SIGNAL 11/⍨pw<5
 kw←'SELECT' 'INSERT' 'UPDATE' 'DELETE' 'FROM' 'SET' 'WHERE'
 kw,←'GROUP BY' 'ORDER BY' 'HAVING' 'AND' 'OR' 'ON'
 kw←(⊂'(SELECT '),' ',¨kw,¨' '
 :While ×⍴sql←(+/∧\sql=' ')↓sql
     :If '('=⊃sql
             ⍝ Indent everything at this level of parens
         at←(-⌿+\'()'∘.=∆clean sql)⍳0
         :If (at+1+(⌽mat)⍳nl)≤pw-ind ⍝ Try to fit short parenthetical expressions on the same line
             mat,←((nl≠¯1↑nl,mat)/' '),at↑sql ⍝? Are they still/more readable?
         :ElseIf at≤pw-ind ⍝ Or perhaps on a single separate line if they don't fit on the same one
             mat,←nl,(ind⍴' '),at↑sql ⍝? Are they still/more readable?
         :Else ⍝ If we have to, split them all up on multiple lines
             mat,←nl,(ind⍴' '),'('
             m←↑nl ∆cut((pw-ind),ind)See(at-2)↑1↓sql
             mat,←nl,nl ∆sew ∆dtr¨↓(-(0,ind)+⍴m)↑m
             mat,←nl,(ind⍴' '),')'
         :EndIf
         sql←at↓sql
     :Else
         mark←kw⍷¨⊂∆uc ∆clean sql←' ',sql ⋄ this←∆dlt((⊃¨mark)⍳1)⊃kw,⊂''
         :If this≡'WHERE' ⋄ at←6
         :ElseIf (this≡'AND')∧'('=⊃∆dlt 4↓sql ⋄ at←4
         :ElseIf (this≡'OR')∧'('=⊃∆dlt 3↓sql ⋄ at←3
         :Else ⋄ at←(1↓⊃∨/mark)⍳1
         :EndIf
         :If ∨/'BETWEEN'⍷∆uc ∆clean at↑sql
                 ⍝ Override the following 'And' because it's part of 'Between'
             t←(∆dlt¨kw)⍳⊂'AND' ⋄ ((t,1+(1↓t⊃mark)⍳1)⊃mark)←0 ⍝ Ignore first 'And' found
             at←(1↓⊃∨/mark)⍳1 ⍝ Recompute next keyword breakpoint
         :EndIf
         phrase←∆dtr at↑sql ⋄ sql←at↓sql ⋄ i←0
         :If pw≥⍴phrase←∆dlt phrase
             mat,←nl,phrase
         :Else ⍝ Wrap long lines as best I can
             :While ×⍴phrase
                 t←((pw-i)+' '=⊃(pw-i)↓phrase)↑phrase ⋄ t←(t=' ')∨(∆clean t)∊',;)' ⋄ t←0⌈(0≠t)⌈.×⍳⍴t
                 t+←pw×t=0 ⋄ mat,←nl,(i⍴' '),∆dlt t↑phrase ⋄ phrase←∆dlt t↓phrase ⋄ i←2×ind
             :EndWhile
         :EndIf
         :Select this
         :Case 'SELECT' ⍝ Indent everything hereafter
             m←↑nl ∆cut((pw-ind),ind)See sql
             mat,←nl,nl ∆sew ∆dtr¨↓(-(0,ind)+⍴m)↑m
             sql←''
         :CaseList 'FROM' 'WHERE'
             t←at↓⊃∨/mark[(∆dlt¨kw)⍳'WHERE' 'GROUP BY' 'ORDER BY' 'HAVING']
             at←¯1+(t∧0=-⌿+\'()'∘.=∆clean sql)⍳1
             m←↑nl ∆cut((pw-ind),ind)See at↑sql
             mat,←(at=0)↓nl,nl ∆sew ∆dtr¨↓(-(0,ind)+⍴m)↑m
             sql←at↓sql
         :Case 'ON' ⍝ Indent just this phrase
             t←at↓⊃∨/mark[(∆dlt¨kw)⍳'WHERE' 'GROUP BY' 'ORDER BY' 'HAVING']
             at←¯1+(t∨<\0>-⌿+\'()'∘.=∆clean sql)⍳1
             m←↑nl ∆cut((pw-ind),ind)See at↑sql
             mat,←nl,nl ∆sew ∆dtr¨↓(-(0,ind)+⍴m)↑m
             sql←at↓sql
         :EndSelect
     :EndIf
 :EndWhile
 mat←(nl=⊃mat)↓mat
     ⍝? Don't break ()'s unless it's a large, complex internal statement (somehow)?
     ⍝? Add support for pw to actually wrap long lines (may need to break on commas [or other non-spaces?])  Already done?
     ⍝? Remove dependence on ∆dmu (only See uses it, and might cause headaches anyway)?
∇

∇ sql←{special}Select fields;t;quote;noquote
     ⍝∇ Generate a SQL Select clause given the database field names to select.
     ⍝
     ⍝ Right argument:
     ⍝   1.  Nested vector of field names, or a single text string if only one field
     ⍝
     ⍝   2.  Nested vector of field names, with any individual field name nested one level
     ⍝       deeper and containing a field/alias pair.  If only one field name is being
     ⍝       passed in, and it includes an alias, the whole list must still be nested to
     ⍝       depth 3, or else this function will assume that the field and its alias are
     ⍝       actually two different field names.
     ⍝
     ⍝   3.  Nested vector of field names, with any individual field name being preceded
     ⍝       by its alias and a "←" symbol within the text string
     ⍝       (i.e. <alias>←<realfieldname>).
     ⍝
     ⍝   4.  2-column matrix of field names in column 1 and aliases in column 2;
     ⍝       any field name with no alias may have a '' as its alias
     ⍝
     ⍝   An advanced option is available on options 2 and 4 to automatically call Math
     ⍝   implicitly.  This can be quite confusing, but is provided for use where it is
     ⍝   required.  To invoke Math functionality, any individual field name in those
     ⍝   structures may be nested one further level to create a nested vector in its
     ⍝   place.  The first item of the vector is the "expression" template for the
     ⍝   calculation and is passed to Math as its left argument and the remaining
     ⍝   nested items (which may themselves be nested as needed by Math) are provided
     ⍝   as its right argument.  The data must continue to be nested and structured as
     ⍝   if there were an alias, but any such alias may be specified as a '' (or left
     ⍝   out in #2) if it is not needed, as long as the argument depth is correct.
     ⍝   Calling Math directly (including the '⍎' option code) is usually much easier
     ⍝   and more readable, but occasionally there may be a requirement to perform this
     ⍝   operation using data structures alone, and this option is provided for that
     ⍝   purpose.
     ⍝
     ⍝ Left argument:
     ⍝   Optional; any special word(s) to insert into the command after the word 'Select',
     ⍝   such as 'Distinct' or 'Top 5'.
     ⍝
     ⍝   If the left argument contains an '⍎' character (as a separate text item),
     ⍝   that signals this function not to call "Q" on any of the field names.
     ⍝
     ⍝ All field names and aliases will be checked for valid names using "Q", unless
     ⍝ the "⍎" option is used in the left argument.  Further, "Q" will itself suppress
     ⍝ quoting any name which itself contains the "⍎" character or is nested an extra
     ⍝ level deeper (the same depth as Math nesting).  A field name of '*' itself is
     ⍝ never quoted by "Q", nor is any expression ending in '(*)'.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Modified 19 December 2015 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 0=⎕NC'special' ⋄ special←0⍴⊂'' ⋄ :EndIf
 :If 1=|≡special ⋄ special←⊂special ⋄ :EndIf
 :If noquote←×≢special←(,,¨special)~⊂''
     noquote←∨/t←∧/¨'⍎'=(1⌈⍴¨special)↑¨special ⋄ special←(~t)/special
 :EndIf
     
 :If 2=⍴⍴fields ⋄ fields←↓fields ⋄ :EndIf
 :If 1≥|≡fields ⋄ fields←⊂fields ⋄ :EndIf ⋄ fields←,,¨fields
 :If ∨/t←1=|≡¨fields ⋄ (t/fields)←∆dtr¨¨⌽¨¯2↑¨'←'∆cut¨t/fields ⋄ :EndIf
 :If ∨/t←2≥|≡¨fields
     :If noquote ⋄ (t/fields)~¨¨←'⍎' ⋄ :Else ⋄ (t/fields)←↓'.'Q↑2↑¨t/fields ⋄ :EndIf
 :EndIf
 :If ∨/t←3≤|≡¨fields
     (t/fields)←(⊂¨((⊃¨1⊃¨t/fields)Math¨1↓¨1⊃¨t/fields)~¨'⍎'),¨1↓¨t/fields
 :EndIf
 :If ∨/t←×≢¨2⊃¨fields←∆dtr¨¨2↑¨fields ⋄ (t/fields)←(⊂' As ')∆sew¨t/fields ⋄ :EndIf
 :If ∨/~t ⋄ ((~t)/fields)←⊃¨(~t)/fields ⋄ :EndIf
     
 t←' '∆sew ∆dlt¨special ⋄ sql←'Select ',t,((×⍴t)/' '),', '∆sew fields
     
     ⍝? A nested field name is getting confused at alias time -- can't reproduce [in older version]
∇

∇ sql←{fields}Set values;t;q;n
     ⍝∇ Build a SQL "Set" phrase, given field names and new values to assign
     ⍝ All field names and new values must be given in a single call
     ⍝
     ⍝ Syntax (alternate forms):
     ⍝   sql ← {fieldnames} Set {newvalues}
     ⍝   sql ← Set ({fieldname} {newvalue}) [({fieldname} {newvalue})] ...
     ⍝   sql ← Set {fieldnames} ,[1.5] {newvalues}  ⍝ As a two-column matrix
     ⍝
     ⍝ A new value of ⎕UCS 0 (ASCII NUL character) or ⎕NULL will set the field to a SQL NULL value.
     ⍝ A new value of ⎕UCS 127 (ASCII DEL character) will set the field to the SQL DEFAULT value.
     ⍝ A doubly-nested (scalar) text value will be taken as a literal formula.
     ⍝ A doubly-nested (vector) of text values will be passed to Math for formula expansion.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Modified 30 November 2016 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 q←'''''' ⋄ :If ×⎕NC'Config' ⋄ q←4⊃Config ⋄ :EndIf ⍝ Default quotes
     
     ⍝ First analyze alternate argument structures
 :If 0=⎕NC'fields'               ⍝ -- Monadic use
     :Select ≢⍴values
     :Case 1                     ⍝ -- Vector
         :If 1≥|≡⊃values         ⍝ -- A single Name:Value pair
             fields values←,¨⊂¨values ⋄ fields←,¨fields
         :Else                   ⍝ -- A nested vector of Name:Value pairs
             'Rank Error'⎕SIGNAL 4/⍨∨/1≠≢¨values
             'Length Error'⎕SIGNAL 5/⍨∨/2≠≢¨values
             fields←,¨1⊃¨values ⋄ values←2⊃¨values
         :EndIf
     :Case 2                     ⍝ -- Matrix
         'Length Error'⎕SIGNAL 5/⍨2≠¯1↑⍴values
         fields values←↓[1]values ⋄ fields←,¨fields
     :Else
         'Rank Error'⎕SIGNAL 4
     :EndSelect
 :Else ⍝ Dyadic use
     :If 1≥|≡fields ⋄ fields←⊂fields ⋄ values←⊂values ⋄ :EndIf
     fields←,,¨fields ⋄ values←,values
     'Length Error'⎕SIGNAL 5/⍨(≢fields)≠≢values
 :EndIf
     
     ⍝ Next check data for proper usage
 'Fields Domain error'⎕SIGNAL 11/⍨∨/~0 2∊⍨10|⎕DR¨fields
 :If ∨/n←6=t←10|⎕DR¨values←(⎕UCS 0)Null values ⋄ n←n\∧/¨∊¨(10|⎕DR¨¨¨1↑¨¨n/values)∊¨¨¨⊂⊂⊂0 2 ⋄ :EndIf
 'Data Domain Error'⎕SIGNAL 11/⍨∨/~n∨t∊0 2,1 3 5 7 9
 'Data Rank Error'⎕SIGNAL 11/⍨∨/((~n)+t←n∨t∊0 2)<≢¨⍴¨values
     
     ⍝ Ok, we're ready to start formatting
 (t/values)←,¨t/values ⋄ ((~t)/values)←⊃¨(~t)/values ⍝ Coerce ranks
 :If ∨/t←(2≤|≡¨values)∧1<≢¨values ⋄ (t/values)←⊂¨(,¨⊃¨t/values)Math¨1↓¨t/values ⋄ :EndIf
 values←((⊂⊂'Null')@{⍵∊⎕UCS 0})((⊂⊂'Default')@{⍵∊⎕UCS 127})values
 :Trap 0 ⋄ values←(⊂q)∆csv¨⊂¨values ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
 sql←' Set ',', '∆sew' = '∆sew('.'Q fields),[1.5]values
∇

∇ {names}Split records;t;type;scalar;types;empty
     ⍝∇ Accept a nested array of data with field names at the top of each column
     ⍝∇ (such as that returned by ADOGet with a '?Select...' command) and split
     ⍝∇ each column into a (nested) vector in semi-global variables named after
     ⍝∇ each field (column header).
     ⍝
     ⍝ Field names will be converted into legal APL variable names, or specific
     ⍝   variable names or alternate variable names may be provided instead.
     ⍝ The field names may be passed as a vector left argument instead of as
     ⍝   the first row of the right argument data matrix.
     ⍝ If the field names are passed as a left argument and the right argument
     ⍝   is a data vector instead of a matrix, then the data is assumed to be
     ⍝   exactly one data record and the resulting variable names will contain
     ⍝   simple (unnested) character vectors and numeric scalars instead of
     ⍝   nested vectors of character vectors and numeric vectors.
     ⍝ If the field names are passed as a left argument and the right argument
     ⍝   is an empty matrix (no records), then the left argument may optionally
     ⍝   also contain data type codes so that appropriately-typed empty arrays
     ⍝   may be used when creating the split variables.  To use this feature,
     ⍝   make the left argument a matrix where [1;] is the field/variable name
     ⍝   list and [2;] is the data type codes to use for each of them.
     ⍝   Data type codes may be ADO field type numbers or may be composed of
     ⍝   0 (for numeric fields) and '' (for character fields) for easier manual
     ⍝   use.  This feature may not be used when Split is called monadically
     ⍝   (with the field names at the top of the data matrix).
     ⍝   If this feature is not used and no records are present, then the data
     ⍝   type of all variables will be text (if called monadically) or will
     ⍝   match that of the first field (if called dyadically).
     ⍝
     ⍝ Examples:
     ⍝   Split ADOGet '?Select Name, Address, Phone From Customers'
     ⍝   Split 'CustName' 'CustAddr' 'CustPhone'⍪ADOGet 'Select Name, Address, Phone From Customers'
     ⍝   'CustName' 'CustAddr' 'CustPhone' Split ADOGet 'Select Name, Address, Phone From Customers'
     ⍝   ('Name' 'Addr' 'Phone',[.5] '' '' 0) Split ADOGet 'Select Name, Address, Phone From Customers'
     ⍝   x←ADOGet '??Select Name, Address, Phone From Customers' ⋄ x[1 2;] Split 2 0↓x
     ⍝
     ⍝ Written about 2008 by Davin Church of Creative Software Design
     ⍝ Modified 22 February 2016 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 types←⍬ ⋄ :If 0=⎕NC'names' ⋄ names←records[1;] ⋄ records←2 0↓(⊂'')⍪records ⋄ :EndIf
 :If 2=⍴⍴names ⋄ :AndIf 2=≢names ⋄ types←names[2;] ⋄ names←names[1;] ⋄ :EndIf
 :If 1=⍴⍴names ⋄ :AndIf ∧/1=≢¨t←⍴¨names ⋄ :AndIf ∧/' '=⊃¨0⍴¨names ⋄ :AndIf ∧/×⊃¨t
 :Else ⋄ 'Invalid structure for field names'⎕SIGNAL 11 ⋄ :EndIf
 :If 1=⍴⍴types ⋄ :AndIf ∧/1≥≢¨t←⍴¨types ⋄ :AndIf ∧/(0≡¨⊃¨0⍴¨types)∨types∊0 ⍬ 1∘.⍴0 ' ' ⋄ :AndIf ∧/(⊃¨t)∊¨⊂0 1
 :Else ⋄ 'Invalid structure for field types'⎕SIGNAL 11 ⋄ :EndIf
 'No data columns'⎕SIGNAL 11/⍨0∊¯1↑⍴records
 :If scalar←1=≢⍴records ⋄ records←(¯2↑1 1,⍴records)⍴records ⋄ :EndIf
 :If 2≠⍴⍴records ⋄ 'Rank Error (Records)'⎕SIGNAL 4 ⋄ :EndIf
 :If empty←(0∊≢records)∧×⍴types←⊃¨types ⍝ Set data prototypes if there are no records but column types have been provided
         ⍝ Create a non-empty data array so columns can be emptied individually later
     t←types∊20 11 136 72 3 2 16 21 19 18 17 6 131 14 5 4 7 133 134,0 ⍝ Allow 0=num & ''=char for flexible typing
     records⍪←('' 0)[1+t] ⍝ Add a dummy record to the empty matrix
 :EndIf
 'Length Error (Names)'⎕SIGNAL 5/⍨(⍴names)≠¯1↑⍴records
     
     ⍝ Prepare the variable names for use and check proper localization
 (((∊names)∊'~∧')/∊names)←'∆' ⋄ (((∊names)∊'. ')/∊names)←'_' ⍝ Convert to legal variable names
 ((~(∊names)∊'∆⍙_¯',⎕D,⎕A,819⌶⎕A)/∊names)←'⍙' ⍝ Convert to legal variable names
 'Domain Error (Names) - Unable to assign'⎕SIGNAL 11/⍨~∧/(⎕NC↑names)∊0 2
     
     ⍝ Data and variables are checked and ready - make the assignments
 records←empty↓¨↓[1]records ⋄ :If scalar ⋄ records←⊃¨records ⋄ :EndIf ⍝ Select output structure
 ⍎(⍕names),'←records' ⍝ Strand-assign all the names at once!
∇

∇ sql←Upd clauses;table;set;where
     ⍝∇ Generate a SQL Update command from its component clauses by passing them
     ⍝∇ to the subroutines Update, Set, & Where and return the results
     ⍝∇ catenated together into a whole SQL command.
     ⍝
     ⍝ Provide arguments in this order (the standard order of a SQL Update statement):
     ⍝       {update-table} {set-values} {where-clause}
     ⍝ Each of these values should be suitable for passing (monadically) to each
     ⍝ of these corresponding functions:  Update, Set, Where
     ⍝
     ⍝ Written 17 December 2014 by Davin Church of Creative Software Design
     ⍝ Updated 31 January 2017 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 'Length Error'⎕SIGNAL 5/⍨(2≤|≡clauses)∧3≠¯1↑1,⍴clauses
 table set where←clauses
 :Trap 0
     :If 1<|≡set ⋄ set←Set set ⋄ :EndIf ⍝ Permit Set to be called before now, as well as not
     sql←(Update table),set,Where where
 :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
∇

∇ sql←Update table
     ⍝∇ Create the beginning of a SQL UPDATE command, listing table(s) to include
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡table ⋄ :If 2=⍴⍴table ⋄ table←↓[2]table ⋄ :Else ⋄ table←⊂table ⋄ :EndIf ⋄ :EndIf
 table←(∆dlt¨,,¨table)~⊂''
 'Missing table name to Update'⎕SIGNAL 11/⍨0∊⍴table
 'Cannot Update joined tables'⎕SIGNAL 11/⍨1<≢table
 'Cannot Update joined tables'⎕SIGNAL 11/⍨∨/'→><≠∘='∊⊃table
 'Cannot Update joined tables'⎕SIGNAL 11/⍨∨/'(,)'∊⊃table
 :Trap 0 ⋄ sql←'Update',5↓From table ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
∇

∇ sql←Values values;q;t;n;rho
     ⍝∇ Create a SQL Values clause, with one or more records of data.
     ⍝ The data items should be in the same order as specified in the
     ⍝ Insert Into clause, or in the table-defined order if none were listed.
     ⍝
     ⍝ A vector produces a Values clause with a single record of data.
     ⍝ A matrix produces a Values clause with multiple records of data.
     ⍝ Note that the multiple-record form is not accepted by all SQL DBs.
     ⍝
     ⍝ A new value of ⎕UCS 0 (ASCII NUL character) or ⎕NULL will set the field to a SQL NULL value.
     ⍝ A new value of ⎕UCS 127 (ASCII DEL character) will set the field to the SQL DEFAULT value.
     ⍝ A doubly-nested text value will be taken as a literal formula rather than data.
     ⍝
     ⍝ Written 15 December 2011 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 q←'''''' ⋄ :If ×⎕NC'Config' ⋄ q←4⊃Config ⋄ :EndIf ⍝ Configurable string quote marks
 :If 1≥|≡values ⋄ :AndIf ' '=⊃0⍴values ⋄ values←⊂,values ⋄ :EndIf
 rho←⍴values←1/values ⋄ values←,((⎕UCS 0)@{⍵∊⎕NULL})values ⍝ Let's do all the checking as a vector
 :If ∨/n←6=t←10|⎕DR¨values ⋄ n←n\∧/¨,¨(10|⎕DR¨¨n/values)∊¨¨⊂⊂0 2 ⋄ :EndIf
 'Data Domain Error'⎕SIGNAL 11/⍨∨/~n∨t∊0 2,1 3 5 7 9
 'Data Rank Error'⎕SIGNAL 4/⍨∨/((~n)+t←n∨t∊0 2)<≢¨⍴¨values
 (t/values)←,¨t/values ⋄ ((~t)/values)←⊃¨(~t)/values ⍝ Coerce ranks
 values←((⊂⊂'Null')@{⍵∊⎕UCS 0})((⊂⊂'Default')@{⍵∊⎕UCS 127})values
 :Trap 0 ⋄ sql←'()'∆Q q', '∆csv rho⍴values ⋄ :Else ⋄ ⎕SIGNAL ∆dmx ⋄ :EndTrap
 :If 1<|≡sql ⋄ sql←', '∆sew sql ⋄ :EndIf ⋄ sql←' Values ',sql
     
     ⍝? Allow use of deeply-nested data to call Math internally?
∇

∇ sql←Where phrases;t
     ⍝∇ Generate a SQL "Where" clause from the argument(s) given.
     ⍝ One or more phrases may be specified in any of the following formats:
     ⍝   (1) A complete "Where" clause (except for the text 'Where' itself)
     ⍝   (2) One or more phrases to be ANDed together to form the final clause
     ⍝   (3) Any phrase (above) may instead be specified as a nested name-value
     ⍝       pair to be given to Is before being further processed.
     ⍝   (4) A two-column matrix of name-value pairs to be passed to Is.
     ⍝ An empty call will produce a '' result (no "Where" keyword)
     ⍝
     ⍝ Written 15 December 2011 by Davin Church and Doug Neman of Creative Software Design
     ⍝ Converted to Dyalog 8 November 2019 by Davin Church of Creative Software Design
     
 :If 1≥|≡phrases ⋄ phrases←,⊂phrases ⋄ :EndIf
 :If 2=⍴⍴phrases ⋄ phrases←↓[2]phrases ⋄ :EndIf ⋄ phrases←,,¨phrases
 :If ∨/t←6=10|⎕DR¨phrases ⍝ Run name/value pairs through Is
     'Length Error'⎕SIGNAL 5/⍨∨/2≠≢¨t/phrases
     (t/phrases)←⊃¨Is/¨t/phrases
 :EndIf
 sql←And ∆dlt¨phrases ⋄ sql←((×⍴sql)/' Where '),sql
∇

∇ q←{m}∆Q x
     ⍝∇ Enclose string(s) in quotes (doubling internal quotes as needed)
     ⍝ Optional left argument specifies quote type ("'", '"', or ''≡none) to use.
     ⍝ Left argument may be two symbols for begin/end (e.g. "[]" or "()").
     ⍝ If left argument does not contain either standard quote type, then imbedded
     ⍝ copies of those characters are not doubled (assuming that they're just
     ⍝ wrapper chars and not true quotes).
     ⍝
     ⍝ Written 23 September 2003 by Davin Church of Creative Software Design
     ⍝ Modified 6 October 2013 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 :If 0=⎕NC'm' ⋄ m←'''' ⋄ :EndIf
 :If 1<|≡x ⋄ q←(⊂m)∆Q¨x ⋄ :Return ⋄ :EndIf ⍝ Handle multiple strings
 :If ×⍴m←,m ⋄ m←2⍴m ⋄ :EndIf
 q←(¯1↓m),((1+(x∊m)∧∨/'''"'∊m)/x),1↓m
∇

∇ clean←{filler}∆clean code;at;this;⎕IO;⎕ML
     ⍝∇ Clean up executable APL code for analysis by blanking out strings and comments
     ⍝ Handles both single and double quotes and nested/overlapping/doubled symbols.
     ⍝ Accepts arguments of any size, rank or depth and returns the same structure.
 ⎕IO←⎕ML←1 ⋄ :If 0=⎕NC'filler' ⋄ filler←' ' ⋄ :EndIf ⋄ filler←⊃filler
 :If 1<|≡code ⋄ clean←filler ∆clean¨code ⋄ :Return ⋄ :EndIf
 :If 1<⍴⍴code←1/code ⋄ clean←↑filler ∆clean¨↓code ⋄ :Return ⋄ :EndIf
 :If ~∨/'''"⍝'∊clean←code ⋄ :Return ⋄ :EndIf ⋄ clean←''
 :While (at←⌊/code⍳'''"⍝')<⍴code
     this←¯1↑clean,←at↑code ⋄ at←(code←at↓code)∊this~'⍝'
     at←(at∧(≠\at)∧~(2⍴this)⍷code)⍳1
     clean,←((at-1)⍴filler),(at≤⍴code)⍴this ⋄ code←at↓code
 :EndWhile
 clean,←code
∇

∇ text←{q}∆csv values;t;u;v;w;x;⎕PP;⎕IO;⎕ML
     ⍝∇ Change an array of values into simple comma-separated text string(s).
     ⍝ Left argument (optional, nested or unnested, 0-2 items):
     ⍝   [1] Quotation mark symbol or symbol-pair (as used by ∇∆Q) [Default = '''']
     ⍝   [2] "Comma" separator symbol(s) (only when nested) [Default = ',']
     ⍝ Right argument: Data to be formatted (Depth ≤ 2[text]; Any rank)
     ⍝ Requires subroutine: ∆Q
     ⍝ Provides features that cannot be replaced by ⎕CSV
     ⍝ Doesn't do anything special with higher-resolution ⎕DR=1287 yet
     ⍝
     ⍝ Written by Davin Church of Creative Software Design
     ⍝ Last modified 18 October 2007 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1 ⋄ ⎕PP←16 ⋄ :If 2≠⎕NC'q' ⋄ q←0⍴⊂'' ⋄ :EndIf
 :If 1≥|≡q ⋄ q←⊂q ⋄ :EndIf ⋄ q←,¨q,(⍴,q)↓'''' ',' ⋄ t←0 ' '⍳⊃¨⊃0⍴⊂values←1/values
 :If ∨/u←,t=1 ⍝ Numeric formatting (can get pretty complicated in extreme cases)
     :If ∨/v←⊃¨(|x←u/,values)<0.000001 ⍝ Avoid use of miniscule exponential notation
         x←⍕¨(u/,values)+v×w←×x ⋄ (v/x)←((v/w<0)⍴¨'-'),¨'0',¨((v/x)⍳¨'1')↓¨v/x
     :Else
         x←⍕¨x ⍝ Nothing too small here - just handle it trivially
     :EndIf
     (('¯'=∊x)/∊x)←'-' ⋄ (u/,values)←x ⍝ Ok, numbers are finally ready
 :EndIf
 :If ∨/u←,t=2 ⋄ (u/,values)←q[1]∆Q¨u/,values ⋄ :EndIf ⍝ Character-string formatting
 :If ∨/u←,t=3 ⋄ (u/,values)←⍕¨⊃¨u/,values ⋄ :EndIf ⍝ Constant-text non-formatting
 text←(⍴2⊃q)↓¨,/(⊂'')⍪(⊂''),q[2],¨values ⍝ Handle arbitrary arrays
 :If 1=⍴⍴values ⋄ text←⊃text ⋄ :Else ⋄ text←1↓[1]text ⋄ :EndIf
∇

∇ words←{delim}∆cut text;⎕IO;⎕ML
     ⍝ Cut a vector into nested pieces at a delimiter, without empties.
     ⍝ The delimiter(s) are optional and default to a space (' '),
     ⍝ NewLine, and LineFeed.
     ⍝
     ⍝ Written before May 1996 by Davin Church of Creative Software Design
     ⍝ Last modified 4 November 2011 by Davin Church of Creative Software Design.
     ⍝ Converted to Dyalog on 12 October 2019 by Davin Church of Creative Software Design.
     
 ⎕IO←⎕ML←1 ⋄ :If 0=⎕NC'delim' ⋄ delim←' ',⎕UCS 13 10 ⋄ :EndIf ⍝ Default delimiters to space & CR/LF
 words←(~text∊delim)⊆,text
∇

∇ words←{delim}∆cuts text;⎕IO;⎕ML
     ⍝ Cut a vector into nested pieces at a delimiter, including empties.
     ⍝ The delimiter(s) are optional and default to a space (' '),
     ⍝ NewLine, and LineFeed.
     ⍝
     ⍝ Written before May 1996 by Davin Church of Creative Software Design.
     ⍝ Last modified 4 November 2011 by Davin Church of Creative Software Design.
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design.
     
 ⎕IO←⎕ML←1 ⋄ :If 0=⎕NC'delim' ⋄ delim←' ',⎕UCS 13 10 ⋄ :EndIf ⍝ Default delimiters to space & CR/LF
 words←1↓¨(1,text∊delim)⊂(1↑delim),text ⍝ Cut it into pieces
∇

∇ trim←{rmv}∆dim data;keep;⎕IO;⎕ML
     ⍝∇ <D>eletes leading, trailing, and all <IM>bedded spaces
     ⍝∇ (or zeros/prototypes/∆dim-prototypes) from the rows of any array.
     ⍝∇ A left argument may be specified with a vector of item values
     ⍝∇ to be removed instead of spaces/zeros/[∆dim-]prototypes.
     ⍝ A "∆dim-prototype", defined as the ∆dim of the prototype,
     ⍝ is additionally removed when the data is nested.
     ⍝ For convenience, if both the left and right arguments are given as
     ⍝ simple (unnested) values, and their datatypes (character or numeric)
     ⍝ are different, then the right argument will be returned unchanged
     ⍝ (not even ravelled, as can otherwise be common for this function).
     ⍝ For instance, this allows the removal of spaces from each item
     ⍝ of a nested array without ravelling any numeric scalars that may
     ⍝ also be found in that array.
     ⍝
     ⍝ Last modified 10 October 2011 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1
 :If 0≠⎕NC'rmv' ⋄ :AndIf ~6∊10|⎕DR¨rmv data ⋄ :AndIf ≠/⊃¨0⍴¨rmv data
     trim←data ⋄ :Return ⍝ Different datatypes - return input unchanged
 :EndIf
 :If 0=⎕NC'rmv' ⋄ :AndIf 1<|≡rmv←1↑0⍴data ⋄ rmv,←∆dim¨rmv ⋄ :EndIf
 keep←~data∊rmv
 trim←+/keep ⋄ trim←trim∘.≥⍳⌈/0,,trim ⋄ trim←(⍴trim)⍴(,trim)\(,keep)/,data
∇

∇ trim←{rmv}∆dlt data;⎕IO;⎕ML;keep
     ⍝∇ <D>eletes <L>eading and <T>railing spaces
     ⍝∇ (or zeros/prototypes/∆dlt-prototypes) from the rows of any array.
     ⍝∇ A left argument may be specified with a vector of item values
     ⍝∇ to be removed instead of spaces/zeros/[∆dlt-]prototypes.
     ⍝ A "∆dlt-prototype", defined as the ∆dlt of the prototype,
     ⍝ is additionally removed when the data is nested.
     ⍝ For convenience, if both the left and right arguments are given as
     ⍝ simple (unnested) values, and their datatypes (character or numeric)
     ⍝ are different, then the right argument will be returned unchanged
     ⍝ (not even ravelled, as can otherwise be common for this function).
     ⍝ For instance, this allows the removal of spaces from each item
     ⍝ of a nested array without ravelling any numeric scalars that may
     ⍝ also be found in that array.
     ⍝
     ⍝ Last modified 10 October 2011 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1
 :If 0≠⎕NC'rmv' ⋄ :AndIf ~6∊10|⎕DR¨rmv data ⋄ :AndIf ≠/⊃¨0⍴¨rmv data
     trim←data ⋄ :Return ⍝ Different datatypes - return input unchanged
 :EndIf
 :If 0=⎕NC'rmv' ⋄ :AndIf 1<|≡rmv←1↑0⍴data ⋄ rmv,←∆dlt¨rmv ⋄ :EndIf
 keep←~data∊rmv ⋄ keep←(∨\keep)∧⌽∨\⌽keep
 trim←+/keep ⋄ trim←trim∘.≥⍳⌈/0,,trim ⋄ trim←(⍴trim)⍴(,trim)\(,keep)/,data
∇

∇ trim←{rmv}∆dmu data;keep;⎕IO;⎕ML
     ⍝∇ <D>eletes leading, trailing, and <MU>ltiple imbedded spaces
     ⍝∇ (or zeros/prototypes/∆dmu-prototypes) from the rows of any array.
     ⍝∇ A left argument may be specified with a vector of item values
     ⍝∇ to be removed instead of spaces/zeros/[∆dmu-]prototypes.
     ⍝ A "∆dmu-prototype", defined as the ∆dmu of the prototype,
     ⍝ is additionally removed when the data is nested.
     ⍝ For convenience, if both the left and right arguments are given as
     ⍝ simple (unnested) values, and their datatypes (character or numeric)
     ⍝ are different, then the right argument will be returned unchanged
     ⍝ (not even ravelled, as can otherwise be common for this function).
     ⍝ For instance, this allows the removal of spaces from each item
     ⍝ of a nested array without ravelling any numeric scalars that may
     ⍝ also be found in that array.
     ⍝
     ⍝ Last modified 10 October 2011 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1
 :If 0≠⎕NC'rmv' ⋄ :AndIf ~6∊10|⎕DR¨rmv data ⋄ :AndIf ≠/⊃¨0⍴¨rmv data
     trim←data ⋄ :Return ⍝ Different datatypes - return input unchanged
 :EndIf
 :If 0=⎕NC'rmv' ⋄ :AndIf 1<|≡rmv←1↑0⍴data ⋄ rmv,←∆dmu¨rmv ⋄ :EndIf
 keep←~data∊rmv ⋄ keep∨←(((-1⌈⍴⍴keep)↑¯1)↓0,keep)∧⌽∨\⌽keep
 trim←+/keep ⋄ trim←trim∘.≥⍳⌈/0,,trim ⋄ trim←(⍴trim)⍴(,trim)\(,keep)/,data
∇

∇ dmx←∆dmx
     ⍝ Create a ⎕SIGNAL argument to propogate upward as much of ⎕DMX as possible
 dmx←⊂{⍵,⍪⎕DMX.⍎¨⍵}'Category' 'EM' 'EN' 'ENX' 'Message'
∇

∇ trim←{rmv}∆dtr data;⎕IO;⎕ML
     ⍝∇ <D>eletes all <TR>ailing spaces
     ⍝∇ (or zeros/prototypes/∆dtr-prototypes) from the rows of any array.
     ⍝∇ A left argument may be specified with a vector of item values
     ⍝∇ to be removed instead of spaces/zeros/[∆dtr-]prototypes.
     ⍝ A "∆dtr-prototype", defined as the ∆dtr of the prototype,
     ⍝ is additionally removed when the data is nested.
     ⍝ For convenience, if both the left and right arguments are given as
     ⍝ simple (unnested) values, and their datatypes (character or numeric)
     ⍝ are different, then the right argument will be returned unchanged
     ⍝ (not even ravelled, as can otherwise be common for this function).
     ⍝ For instance, this allows the removal of spaces from each item
     ⍝ of a nested array without ravelling any numeric scalars that may
     ⍝ also be found in that array.
     ⍝
     ⍝ Last modified 10 October 2011 by Davin Church of Creative Software Design
     ⍝ Converted to Dyalog 12 October 2019 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1
 :If 0≠⎕NC'rmv' ⋄ :AndIf ~6∊10|⎕DR¨rmv data ⋄ :AndIf ≠/⊃¨0⍴¨rmv data
     trim←data ⋄ :Return ⍝ Different datatypes - return input unchanged
 :EndIf
 :If 0=⎕NC'rmv' ⋄ :AndIf 1<|≡rmv←1↑0⍴data ⋄ rmv,←∆dtr¨rmv ⋄ :EndIf
 trim←((-1⌈⍴⍴data)↑-(¯1↑1,⍴data)⌊⌊/,+/∧\⌽data∊rmv)↓data
∇

∇ text←{thread}∆sew values;t;u;v;w;x;⎕PP;⎕IO;⎕ML
     ⍝∇ Join together a list (or array) of values into text string(s) separated by delimiters.
     ⍝ Left argument (required): Character or string used to join adjacent values together.
     ⍝ Right argument: Data to be formatted (Depth ≤ 2[text]; Any rank)
     ⍝ (This is a variant of the ∆csv utility.)
     ⍝
     ⍝ Written 31 December 2007 by Davin Church of Creative Software Design
     
 ⎕IO←⎕ML←1 ⋄ ⎕PP←15
 :If 2≠⎕NC'thread' ⋄ thread←'' ⋄ :EndIf ⋄ thread←∊thread ⍝ Thread used for sewing
 t←0 ' '⍳⊃¨⊃0⍴⊂values←1/values
 :If ∨/u←,t=1 ⍝ Numeric formatting (can get pretty complicated in extreme cases)
     :If ∨/v←(|x←u/,values)<0.000001 ⍝ Avoid use of miniscule exponential notation
         x←⍕¨(u/,values)+v×w←×x ⋄ (v/x)←((v/w<0)⍴¨'-'),¨'0',¨((v/x)⍳¨'1')↓¨v/x
     :Else
         x←⍕¨x ⍝ Nothing too small here - just handle it trivially
     :EndIf
     (('¯'=∊x)/∊x)←'-' ⋄ (u/,values)←x ⍝ Ok, numbers are finally ready
 :EndIf
 :If ∨/u←,t=2 ⋄ (u/,values)←⍕¨u/,values ⋄ :EndIf ⍝ Character-string formatting
 :If ∨/u←,t=3 ⋄ (u/,values)←⍕¨⊃¨u/,values ⋄ :EndIf ⍝ Constant-text non-formatting
 text←(⍴thread)↓¨,/(⊂'')⍪(⊂''),(⊂thread),¨values ⍝ Handle arbitrary arrays
 :If 1=⍴⍴values ⋄ text←⊃text ⋄ :Else ⋄ text←1↓[1]text ⋄ :EndIf
∇

 ∆uc←{1(819⌶)⍵}

:EndNamespace 
