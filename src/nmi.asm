;
; nmi - NMI (vertical retrace) handler for robotfindskitten
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

; NMI commands
CMD_EOF			= 0	; no args
CMD_POKE		= 1	; CPU address (2, low byte first), byte
CMD_COPY		= 2	; PPU address (2, high byte first),
				; count (1), data
CMD_FILL		= 3	; PPU address (2, high byte first),
				; count (1) (#0 == 256 bytes), byte
CMD_SCATTER		= 4	; count (1), [PPU address (2, high byte first),
				; byte]

.section zeropage
cmd_off		.byte ?
nmi_addr	.word ?
.send

.section fixed
nmi_table
	; On entry: cmd_off loaded into Y
	; On exit: cmd_off updated
	.word nmi_poke - 1
	.word nmi_copy - 1
	.word nmi_fill - 1
	.word nmi_scatter - 1

nmi	.proc
	pha		; push A
	txa		; <- X
	pha		; push X
	tya		; <- Y
	pha		; push Y

	; see if we should run
	lda nmi_ready	; get ready flag
	beq done	; test
	.cp #0, nmi_ready ; clear ready flag

	; walk command buffer
	ldy #0		; load new command offset
	sty cmd_off	; and store it
	beq +		; start loop
-	jsr cmd_dispatch; dispatch
	ldy cmd_off	; load offset into buffer
+	lda cmd_buf,y	; get command
	bne -		; continue until command 0

	; reset scroll after update
	bit PPUSTATUS	; clear address latch
	lda #0
	sta PPUSCROLL
	sta PPUSCROLL

	; return
done	pla		; pop Y
	tay		; -> Y
	pla		; pop X
	tax		; -> X
	pla		; pop A
	rti
	.pend

; dispatch a command
; A - command byte
cmd_dispatch .proc
	asl		; double command for table offset
	tax		; copy to X
	lda nmi_table - 1,x ; get high byte of subroutine addr
	pha		; push it
	lda nmi_table - 2,x ; get low byte
	pha		; push it
	rts		; call
	.pend

nmi_poke .proc
	iny		; increment offset
	lda cmd_buf,y	; address low byte
	sta nmi_addr	; store it
	iny		; increment offset
	lda cmd_buf,y	; address high byte
	sta nmi_addr + 1 ; store it
	iny		; increment offset
	lda cmd_buf,y	; data byte
	iny		; increment offset
	ldx #0		; offset for indirect addressing
	sta (nmi_addr,x) ; store the byte
	sty cmd_off	; store offset
	rts
	.pend

nmi_copy .proc
	; set nametable address
	bit PPUSTATUS	; clear address latch
	iny		; increment offset
	lda cmd_buf,y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda cmd_buf,y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset

	; get counter
	lda cmd_buf,y	; get counter
	tax		; put in X
	iny		; increment offset

	; write data until multiple of 8 bytes remaining
	bpl +		; start loop
-	lda cmd_buf,y	; load byte
	sta PPUDATA	; write it
	dex		; decrement remaining count
	iny		; increment offset
	txa		; copy counter to A
+	and #$07	; continue until a multiple of 8 bytes
	bne -		; repeat until done

	; write data 8 bytes at a time
	txa		; get remaining count
	beq done	; skip if already done
	lsr		; divide by 8
	lsr
	lsr
	tax		; and put it back
-	lda cmd_buf,y	; load byte 1
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 2
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 3
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 4
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 5
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 6
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 7
	sta PPUDATA	; write it
	iny		; increment offset
	lda cmd_buf,y	; load byte 8
	sta PPUDATA	; write it
	iny		; increment offset
	dex		; decrement count of remaining blocks
	bne -		; repeat until done

	; update cmd_off
done	sty cmd_off	; store offset
	rts
	.pend

nmi_fill .proc
	; set nametable address
	bit PPUSTATUS	; clear address latch
	iny		; increment offset
	lda cmd_buf,y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda cmd_buf,y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset

	; get counter and fill byte
	lda cmd_buf,y	; get counter
	tax		; put in X
	iny		; increment offset
	lda cmd_buf,y	; get fill byte
	iny		; increment offset
	sty cmd_off	; save offset
	tay		; put fill byte in Y

	; bypass early exhaustion checks for count == 0 (256 bytes)
	cpx #0		; count == 0?
	beq unroll	; go directly to main loop

	; write data until multiple of 16 bytes remaining
	jmp +		; start loop
-	sty PPUDATA	; write byte
	dex		; decrement remaining count
	txa		; copy counter to A
+	and #$0f	; continue until a multiple of 16 bytes
	bne -		; repeat until done

	; write data 16 bytes at a time
	txa		; get remaining count
	beq done	; skip if already done
	lsr		; divide by 16
	lsr
	lsr
	lsr
	tax		; and put it back
unroll	sty PPUDATA	; write byte 1
	sty PPUDATA	; write byte 2
	sty PPUDATA	; write byte 3
	sty PPUDATA	; write byte 4
	sty PPUDATA	; write byte 5
	sty PPUDATA	; write byte 6
	sty PPUDATA	; write byte 7
	sty PPUDATA	; write byte 8
	sty PPUDATA	; write byte 9
	sty PPUDATA	; write byte 10
	sty PPUDATA	; write byte 11
	sty PPUDATA	; write byte 12
	sty PPUDATA	; write byte 13
	sty PPUDATA	; write byte 14
	sty PPUDATA	; write byte 15
	sty PPUDATA	; write byte 16
	dex		; decrement remaining count
	bne unroll	; repeat until done

done	rts
	.pend

nmi_scatter .proc
	; get counter
	iny		; increment offset
	lda cmd_buf,y	; get counter
	tax		; put in X
	iny		; increment offset

	; copy items
-	bit PPUSTATUS	; clear address latch
	lda cmd_buf,y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda cmd_buf,y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset
	lda cmd_buf,y	; get data byte
	sta PPUDATA	; write it
	iny		; increment offset
	dex		; decrement counter
	bne -		; continue until done

	; update cmd_off
	sty cmd_off	; store offset
	rts
	.pend

.send
