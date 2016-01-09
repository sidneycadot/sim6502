;
; File generated by cc65 v 2.14.0
;
;	.fopt		compiler,"cc65 v 2.14.0"
;	.setcpu		"6502"
;	.smart		on
;	.autoimport	on
;	.case		on
;	.debuginfo	off
;	.importzp	sp, sreg, regsave, regbank
;	.importzp	tmp1, tmp2, tmp3, tmp4, ptr1, ptr2, ptr3, ptr4
;	.macpack	longbranch

            .export _test

; ---------------------------------------------------------------
; unsigned int __near__ test (void)
; ---------------------------------------------------------------


;     Bit No.       7   6   5   4   3   2   1   0
;                   S   V       B   D   I   Z   C

ZERO_PAGE_SAVE = $5000

WALK_P      = $80
WALK_OP1    = $81
WALK_OP2    = $82
RESULT_P    = $83
RESULT_A    = $84

                .segment "CODE"

                .proc _test

                jsr save_page_zero

                lda #0
                sta WALK_P
                sta WALK_OP1
                sta WALK_OP2
testloop:

show_curr:
                ldy #39
                lda WALK_P
                sta (88),y
                ldy #79
                lda WALK_OP1
                sta (88),y
                ldy #119
                lda WALK_OP2
                sta (88),y

adc_test:
                lda WALK_P
                pha
                lda WALK_OP1
                plp
                adc WALK_OP2
                php
                sta RESULT_A
                pla
                sta RESULT_P
check_z:
                jmp bad_exit
                lda RESULT_P
                and #$02
                bne @FLAG_1
@FLAG_0:
                lda RESULT_A
                bne @ok
                jmp bad_exit
@FLAG_1:
                lda RESULT_A
                beq @ok
                jmp bad_exit
@ok:

next:
                inc WALK_P
                lda WALK_P
                bne testloop
                inc WALK_OP1
                lda WALK_OP1
                bne testloop
                inc WALK_OP2
                lda WALK_OP2
                bne testloop

                ; all done
good_exit:
                ldx #0
                lda #0
                jmp restore_page_zero

                ; bad exit
bad_exit:
                lda #34
                sta 712
                ldx #0
                lda #1
                jmp restore_page_zero

                .endproc

                ; ---------------------------------------

                .proc save_page_zero

                php
                pha
                txa
                pha

                ldx #0
@1:             lda $80,x
                sta ZERO_PAGE_SAVE,x
                inx
                cpx #$20
                bne @1

                pla
                tax
                pla
                plp

                rts

                .endproc

                ; ---------------------------------------

                .proc restore_page_zero

                php
                pha
                txa
                pha

                ldx #0
@1:             lda ZERO_PAGE_SAVE,x
                sta $80,x
                inx
                cpx #$20
                bne @1

                pla
                tax
                pla
                plp

                rts

                .endproc