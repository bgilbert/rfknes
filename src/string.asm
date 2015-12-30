;
; string - String handling
;
; Copyright (C) 2015 Benjamin Gilbert
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;

; Define new string array (number of strings, followed by null-terminated
; strings)
; args: string_name, bank, [string]
strings	.segment
\1_bank = \2
	.section bank\2
\1_addr	.byte len(\3)
	.for _cur = 0, _cur < len(\3), _cur = _cur + 1
	.null \3[_cur]
	.next
	.send
	.endm

; Define new string (double-null-terminated)
; args: string_name, bank, string
string	.segment
	.strings \1, \2, [\3]
	.endm

; Record string in string table
; args: string_name
stringentry .macro
.if MAPPER
	.byte <\1_addr
	.byte ((\1_addr >> 6) & $fc) | \1_bank
.else
	.word \1_addr
.fi
	.endm
