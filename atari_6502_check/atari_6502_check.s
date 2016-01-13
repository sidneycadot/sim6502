
                .import __RAM_START__
                .import __RAM_LAST__

POKMSK          = $10
LMARGN          = $52
SAVMSC          = $58
COLOR1          = $2c5
COLOR2          = $2c6
COLOR4          = $2c8
RUNAD           = $2e0
CRSINH          = $2f0
ICCOM           = $342
ICBAL           = $344
ICBAH           = $345
ICBLL           = $348
ICBLH           = $349
ICAX1           = $34a
IRQEN           = $d20e
NMIEN           = $d40e
CIOV            = $e456

                .segment "HEADER"

                .word $ffff
                .word __RAM_START__
                .word __RAM_LAST__ - 1

                .segment "TRAILER"

                .word RUNAD
                .word RUNAD + 1
                .word start

                .segment "RODATA"

FILENAME:       .byte "H:OPCDATA.BIN",  $9b
START_GUI_MSG:  .byte $7d, "Hello, world!", $9b, "Second line", $9b
START_GUI_MSG_SIZE = * - START_GUI_MSG

WRITE_FILE_FLAG: .byte 0

                .segment "BSS"

REC_START       = *

WALK_P:         .res 1
WALK_OP1:       .res 1
WALK_OP2:       .res 1

                .if 0

RESULT_CLC_P:   .res 1
RESULT_SEC_P:   .res 1
RESULT_CLI_P:   .res 1
RESULT_SEI_P:   .res 1
RESULT_CLV_P:   .res 1
RESULT_CLD_P:   .res 1
RESULT_SED_P:   .res 1

RESULT_NOP_P:   .res 1

RESULT_BPL_P:   .res 1
RESULT_BMI_P:   .res 1
RESULT_BVC_P:   .res 1
RESULT_BVS_P:   .res 1
RESULT_BCC_P:   .res 1
RESULT_BCS_P:   .res 1
RESULT_BNE_P:   .res 1
RESULT_BEQ_P:   .res 1

RESULT_TAX_X:   .res 1
RESULT_TAX_P:   .res 1

RESULT_TXA_A:   .res 1
RESULT_TXA_P:   .res 1

RESULT_TAY_Y:   .res 1
RESULT_TAY_P:   .res 1

RESULT_TYA_A:   .res 1
RESULT_TYA_P:   .res 1

;RESULT_TXS_S:   .res 1
;RESULT_TXS_P:   .res 1

;RESULT_TSX_X:   .res 1
;RESULT_TSX_P:   .res 1

RESULT_INX_X:   .res 1
RESULT_INX_P:   .res 1

RESULT_INY_Y:   .res 1
RESULT_INY_P:   .res 1

RESULT_DEX_X:   .res 1
RESULT_DEX_P:   .res 1

RESULT_DEY_Y:   .res 1
RESULT_DEY_P:   .res 1

RESULT_TSX_X:   .res 1
RESULT_TSX_P:   .res 1

                .endif

RESULT_ORA_A:   .res 1
RESULT_ORA_P:   .res 1

RESULT_AND_A:   .res 1
RESULT_AND_P:   .res 1

RESULT_EOR_A:   .res 1
RESULT_EOR_P:   .res 1

RESULT_ADC_A:   .res 1
RESULT_ADC_P:   .res 1

RESULT_STA_MEM: .res 1
RESULT_STA_P:   .res 1

RESULT_LDA_A:   .res 1
RESULT_LDA_P:   .res 1

RESULT_CMP_A:   .res 1
RESULT_CMP_P:   .res 1

RESULT_SBC_A:   .res 1
RESULT_SBC_P:   .res 1

RESULT_BIT_A:   .res 1
RESULT_BIT_P:   .res 1

REC_SIZE        = * - REC_START

CRC32:          .res 4

CRCTABLE0:      .res 256
CRCTABLE1:      .res 256
CRCTABLE2:      .res 256
CRCTABLE3:      .res 256

                .segment "CODE"

start:          jsr start_gui

                jsr init_crc_table
                jsr init_crc

                lda WRITE_FILE_FLAG
                beq @skip
                jsr open_file
@skip:

                lda #0
                sta WALK_P
                sta WALK_OP1
                sta WALK_OP2

loop:
                jsr update_record

                jsr update_record_crc

                jsr show_progress

                lda WRITE_FILE_FLAG
                beq @skip
                jsr write_record
@skip:
                lda WALK_P
                clc
                adc #1
                sta WALK_P
                lda WALK_OP1
                adc #0
                sta WALK_OP1
                lda WALK_OP2
                adc #0
                sta WALK_OP2
                ora WALK_OP1
                ora WALK_P
                bne loop
wrap_up:
                lda WRITE_FILE_FLAG
                beq @skip
                jsr invert_crc
                jsr write_crc
                jsr close_file
@skip:

finish:
                lda #14
                sta COLOR4
@loop:          jmp @loop

start_gui:
                php
                pha
                txa
                pha
                tya
                pha

                lda #0
                sta COLOR2
                lda #14
                sta COLOR1
                lda #1
                sta CRSINH
                lda #0
                sta LMARGN

                ldx #0
                lda #11
                sta ICCOM,x
                lda #<START_GUI_MSG
                sta ICBAL,x
                lda #>START_GUI_MSG
                sta ICBAH,x
                lda #<START_GUI_MSG_SIZE
                sta ICBLL,x
                lda #>START_GUI_MSG_SIZE
                sta ICBLH,x

                jsr CIOV
                bpl @ok

                lda #34
                sta COLOR4
@error:         jmp @error

@ok:
                pla
                tay
                pla
                tax
                pla
                plp
                rts

open_file:      php
                pha
                txa
                pha
                tya
                pha

                ldx #$10

                lda #3
                sta ICCOM,x
                lda #<FILENAME
                sta ICBAL,x
                lda #>FILENAME
                sta ICBAH,x
                lda #8
                sta ICAX1,x

                jsr CIOV

                bpl @ok
                lda #34
                sta COLOR4
@error:         jmp @error
@ok:
                pla
                tay
                pla
                tax
                pla
                plp

                rts

close_file:     php
                pha
                txa
                pha
                tya
                pha

                ldx #$10

                lda #12
                sta ICCOM,x

                jsr CIOV

                bpl @ok
                lda #34
                sta COLOR4
@error:         jmp @error
@ok:

                pla
                tay
                pla
                tax
                pla
                plp

                rts

write_record:   php
                pha
                txa
                pha
                tya
                pha

                ldx #$10

                lda #11
                sta ICCOM,x
                lda #<REC_START
                sta ICBAL,x
                lda #>REC_START
                sta ICBAH,x
                lda #<REC_SIZE
                sta ICBLL,x
                lda #>REC_SIZE
                sta ICBLH,x

                jsr CIOV

                bpl @ok
                lda #34
                sta COLOR4
@error:         jmp @error
@ok:
                pla
                tay
                pla
                tax
                pla
                plp

                rts

write_crc:      php
                pha
                txa
                pha
                tya
                pha

                ldx #$10

                lda #11
                sta ICCOM,x
                lda #<CRC32
                sta ICBAL,x
                lda #>CRC32
                sta ICBAH,x
                lda #<4
                sta ICBLL,x
                lda #>4
                sta ICBLH,x

                jsr CIOV

                bpl @ok
                lda #34
                sta COLOR4
@error:         jmp @error
@ok:
                pla
                tay
                pla
                tax
                pla
                plp

                rts

update_record:  php
                pha
                txa
                pha
                tya
                pha

                ; Disable interrupts

                lda NMIEN
                pha             ; save NMIEN on stack
                lda #0
                sta NMIEN
                sta IRQEN

                .if 0

                ; Test CLC (save resulting P register)

                lda WALK_P
                pha
                plp
                clc
                php
                pla
                sta RESULT_CLC_P

                ; Test SEC (save resulting P register)

                lda WALK_P
                pha
                plp
                sec
                php
                pla
                sta RESULT_SEC_P

                ; Test CLI (save resulting P register)

                lda WALK_P
                pha
                plp
                cli
                php
                pla
                sta RESULT_CLI_P

                ; Test SEI (save resulting P register)

                lda WALK_P
                pha
                plp
                sei
                php
                pla
                sta RESULT_SEI_P

                ; Test CLV (save resulting P register)

                lda WALK_P
                pha
                plp
                clv
                php
                pla
                sta RESULT_CLV_P

                ; Test CLD (save resulting P register)

                lda WALK_P
                pha
                plp
                cld
                php
                pla
                sta RESULT_CLD_P

                ; Test SED (save resulting P register)

                lda WALK_P
                pha
                plp
                sed
                php
                pla
                sta RESULT_SED_P

                ; Test NOP (save resulting P register)

                lda WALK_P
                pha
                plp
                nop
                php
                pla
                sta RESULT_NOP_P

                ; Test BPL (save resulting P register)

                lda WALK_P
                pha
                plp
                bpl @branch_bpl
@branch_bpl:    php
                pla
                sta RESULT_BPL_P

                ; Test BMI (save resulting P register)

                lda WALK_P
                pha
                plp
                bmi @branch_bmi
@branch_bmi:    php
                pla
                sta RESULT_BMI_P

                ; Test BVC (save resulting P register)

                lda WALK_P
                pha
                plp
                bvc @branch_bvc
@branch_bvc:    php
                pla
                sta RESULT_BVC_P

                ; Test BVS (save resulting P register)

                lda WALK_P
                pha
                plp
                bvs @branch_bvs
@branch_bvs:    php
                pla
                sta RESULT_BVS_P

                ; Test BCC (save resulting P register)

                lda WALK_P
                pha
                plp
                bcc @branch_bcc
@branch_bcc:    php
                pla
                sta RESULT_BCC_P

                ; Test BCS (save resulting P register)

                lda WALK_P
                pha
                plp
                bcs @branch_bcs
@branch_bcs:    php
                pla
                sta RESULT_BCS_P

                ; Test BNE (save resulting P register)

                lda WALK_P
                pha
                plp
                bne @branch_bne
@branch_bne:    php
                pla
                sta RESULT_BNE_P

                ; Test BEQ (save resulting P register)

                lda WALK_P
                pha
                plp
                beq @branch_beq
@branch_beq:    php
                pla
                sta RESULT_BEQ_P

                ; Test TAX (A = OP1; save resulting X and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                tax
                php
                stx RESULT_TAX_X
                pla
                sta RESULT_TAX_P

                ; Test TXA (X = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                ldx WALK_OP1
                plp
                txa
                php
                sta RESULT_TXA_A
                pla
                sta RESULT_TXA_P

                ; Test TAY (A = OP1; save resulting Y and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                tay
                php
                sty RESULT_TAY_Y
                pla
                sta RESULT_TAY_P

                ; Test TYA (Y = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                ldy WALK_OP1
                plp
                tya
                php
                sta RESULT_TYA_A
                pla
                sta RESULT_TYA_P

                .endif

                ; Test TXS (X = OP1; save resulting S and P registers).
                ; This one is complicated because the "php" following the 'txs' instruction will end up on the stack at $100,x.
                ; We need to save the old stack value and the value present there and restore it later.

                tsx                ; Save stack pointer in Y
                txa
                tay

                lda WALK_P
                pha
                sta SAVE_S
                ldx WALK_OP1
                lda $100,x         ; Save value that will be overwritten by the "php" that follows the "txs" in Y register.
                sta RESULT_TXS_S   ; Abuse as temp storage for value to be overwritten.
                plp                ; Prepare P register.
                txs                ; Instruction under test.
                php                ; This overwrites $100,x and changes S! But it's the only way to save the P register untouched.
                pla                ; Retrieve P result and undo change to S.
                sta RESULT_TXS_P   ; Save P register result.
                tsx                ; Get resulting stack pointer in X (will save to 'RESULT_TXS_S', below).
                lda RESULT_TXS_S   ; The stack value to be restored.
                pha                ; Restore the stack value that was brutally overwritten by the 'php'.
                stx RESULT_TXS_S

                tya                ; Restore stack pointer.
                tax
                txs

                ; Test TSX (S = OP1; save resulting X and P registers).
                ; This one is complicated because the "php" following the 'tsx' instruction will end up on the stack at $100,OP1

                ;lda WALK_P
                ;pha
                ;ldy WALK_OP1
                ;plp
                ;tya
                ;php
                ;sta RESULT_TSX_X
                ;pla
                ;sta RESULT_TSX_P

                ; Test INX (X = OP1; save resulting X and P registers)

                lda WALK_P
                pha
                ldx OP1
                plp
                inx
                php
                stx RESULT_INX_X
                pla
                sta RESULT_INX_P

                ; Test INY (Y = OP1; save resulting Y and P registers)

                lda WALK_P
                pha
                ldy OP1
                plp
                iny
                php
                sty RESULT_INY_Y
                pla
                sta RESULT_INY_P

                ; Test DEX (X = OP1; save resulting X and P registers)

                lda WALK_P
                pha
                ldx OP1
                plp
                dex
                php
                stx RESULT_DEX_X
                pla
                sta RESULT_DEX_P

                ; Test DEY (Y = OP1; save resulting Y and P registers)

                lda WALK_P
                pha
                ldy OP1
                plp
                dey
                php
                sty RESULT_DEY_Y
                pla
                sta RESULT_DEY_P

                ; Test ASL accu (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                plp
                asl a
                php
                sta RESULT_ASL_ACCU_A
                pla
                sta RESULT_ASL_ACCU_P

                ; Test ROL accu (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                plp
                rol a
                php
                sta RESULT_ROL_ACCU_A
                pla
                sta RESULT_ROL_ACCU_P

                ; Test LSR accu (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                plp
                lsr a
                php
                sta RESULT_LSR_ACCU_A
                pla
                sta RESULT_LSR_ACCU_P

                ; Test ROR accu (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                plp
                ror a
                php
                sta RESULT_ROR_ACCU_A
                pla
                sta RESULT_ROR_ACCU_P

                ; Test ASL mem (MEM = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                sta RESULT_ASL_MEM_A
                plp
                asl RESULT_ASL_MEM_A
                php
                pla
                sta RESULT_ASL_MEM_P

                ; Test ROL mem (MEM = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                sta RESULT_ROL_MEM_A
                plp
                rol RESULT_ROL_MEM_A
                php
                pla
                sta RESULT_ROL_MEM_P

                ; Test LSR mem (MEM = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                sta RESULT_LSR_MEM_A
                plp
                lsr RESULT_LSR_MEM_A
                php
                pla
                sta RESULT_LSR_MEM_P

                ; Test ROR mem (MEM = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda OP1
                sta RESULT_ROR_MEM_A
                plp
                ror RESULT_ROR_MEM_A
                php
                pla
                sta RESULT_ROR_MEM_P

                ; Test ORA (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                ora WALK_OP2
                php
                sta RESULT_ORA_A
                pla
                sta RESULT_ORA_P

                ; Test AND (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                and WALK_OP2
                php
                sta RESULT_AND_A
                pla
                sta RESULT_AND_P

                ; Test EOR (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                eor WALK_OP2
                php
                sta RESULT_EOR_A
                pla
                sta RESULT_EOR_P

                ; Test ADC (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                adc WALK_OP2
                php
                sta RESULT_ADC_A
                pla
                sta RESULT_ADC_P

                ; Test STA (A = OP1; save resulting memory and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                sta RESULT_STA_MEM
                php
                ; lda/sta RESULT_STA_A would be silly
                pla
                sta RESULT_STA_P

                ; Test LDA

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                lda WALK_OP1
                php
                sta RESULT_LDA_A
                pla
                sta RESULT_LDA_P

                ; Test CMP (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                cmp WALK_OP2
                php
                sta RESULT_CMP_A
                pla
                sta RESULT_CMP_P

                ; Test SBC (A = OP1; save resulting A and P registers)

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                sbc WALK_OP2
                php
                sta RESULT_SBC_A
                pla
                sta RESULT_SBC_P

                ; Test BIT

                lda WALK_P
                pha
                lda WALK_OP1
                plp
                bit WALK_OP2
                php
                sta RESULT_BIT_A
                pla
                sta RESULT_BIT_P

                ; Restore interrupts

                lda POKMSK
                sta IRQEN
                pla
                sta NMIEN

                pla
                tay
                pla
                tax
                pla
                plp

                rts

update_record_crc:

                php
                pha
                txa
                pha

                ldx #0
@loop:          lda REC_START,x
                jsr update_crc
                inx
                cpx #REC_SIZE
                bne @loop

                pla
                tax
                pla
                plp

                rts

init_crc_table:
                php
                pha
                txa
                pha
                tya
                pha

                ldy #0
@byte_loop:
                sty CRC32 + 0
                lda #0
                sta CRC32 + 1
                sta CRC32 + 2
                sta CRC32 + 3

                ldx #8
@bit_loop:
                sec
                ror CRC32 + 3
                ror CRC32 + 2
                ror CRC32 + 1
                ror CRC32 + 0
                bcs @skipxor

                lda #$20
                eor CRC32 + 0
                sta CRC32 + 0
                lda #$83
                eor CRC32 + 1
                sta CRC32 + 1
                lda #$B8
                eor CRC32 + 2
                sta CRC32 + 2
                lda #$ED
                eor CRC32 + 3
                sta CRC32 + 3
@skipxor:
                dex
                bne @bit_loop

                lda CRC32 + 0
                sta CRCTABLE0,y
                lda CRC32 + 1
                sta CRCTABLE1,y
                lda CRC32 + 2
                sta CRCTABLE2,y
                lda CRC32 + 3
                sta CRCTABLE3,y

                iny
                bne @byte_loop

                pla
                tay
                pla
                tax
                pla
                plp
                rts

init_crc:       php
                pha

                lda #0
                sta CRC32 + 0
                sta CRC32 + 1
                sta CRC32 + 2
                sta CRC32 + 3

                pla
                plp

                rts

update_crc:     php
                pha
                eor CRC32 + 0
                sta CRC32 + 0
                txa
                pha

                ldx CRC32 + 0

                lda CRCTABLE0,x
                eor CRC32 + 1
                sta CRC32 + 0

                lda CRCTABLE1,x
                eor CRC32 + 2
                sta CRC32 + 1

                lda CRCTABLE2,x
                eor CRC32 + 3
                sta CRC32 + 2

                lda CRCTABLE3,x
                sta CRC32 + 3

                pla
                tax
                pla
                plp

                rts

invert_crc:     php
                pha
                txa
                pha

                ldx #0
@loop:
                lda CRC32,x
                eor #$ff
                sta CRC32,x

                inx
                cpx #4
                bne @loop

                pla
                tax
                pla
                plp

                rts

show_progress:  php
                pha
                tya
                pha

                lda WALK_P
                ldy #60
                jsr show_hex
                lda WALK_OP1
                ldy #63
                jsr show_hex
                lda WALK_OP2
                ldy #66
                jsr show_hex

                ldy #70
                lda CRC32 + 3
                jsr show_hex
                ldy #72
                lda CRC32 + 2
                jsr show_hex
                ldy #74
                lda CRC32 + 1
                jsr show_hex
                ldy #76
                lda CRC32 + 0
                jsr show_hex

                pla
                tay
                pla
                plp
                rts

                ; 0 .. 9: 16 .. 25
                ; A .. F: 33 .. 38

show_hex:       php
                pha
                lsr
                lsr
                lsr
                lsr
                cmp #10
                bcc @1
                adc #6
@1:             adc #16
                sta (SAVMSC),y
                pla
                pha
                and #15
                cmp #10
                bcc @2
                adc #6
@2:             adc #16
                iny
                sta (SAVMSC),y
                dey
                pla
                plp
                rts
