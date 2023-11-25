.label charBase = $3000
.label sinBase = $3400
.label spriteBase = $3800



* = $02 "zeropage" virtual
//scrolltextPointer:		.word $00
bounceOn: .byte 0
colourOn: .byte 0
sinPosition: .byte 0
nextchar: .byte 0
spriteColour: .byte 0

* = $0801

BasicUpstart2(Start)


Start: {

	    //	sei 

		//Stop CIA interrupts
		// lda #$7f
		// sta $dc0d
		// sta $dd0d

		//Bank out BASIC + Kernal
		// lda #$35
		// sta $01

		//Set F/B colors and clear screen to black 
		lda #$00
        sta bounceOn
        sta colourOn
        sta sinPosition
		sta $d020
		sta $d021
        lda #1
        sta spriteColour

        lda #<scrolltext
        sta PutCharInSprite7.scrolltextPointer
        lda #>scrolltext
        sta PutCharInSprite7.scrolltextPointer+1



		jsr ClearScreen

        ldx #7
        stx nextchar    // while we have 7 .. make use of it :)
        ldy #SpriteStart/64 + 7
    setSpriteBase:
        tya   
        sta $7f8,x      // set sprite base
        lda #0
        sta $d027,x     // set sprite colour to black
        dey
        dex
        bpl setSpriteBase

        lda #$ff
        sta $d01d       //set X double width
        lda #0
        sta $d017       // set Y to normal height

        ldx #0
        

    setSpriteXY:
        sta $d000,x     //set sprite X
        tay             // save a
        lda sinBase     // sprite Y initially row 128 (see sin table)
        sta $d001,x     //set sprite Y
        tya             // get a back
        clc
        adc #48        // move sprite X along 48 pixels (double width)
        inx
        inx             // move to next X,Y pair
        cpx #$10        // have we done all 8?
        bne setSpriteXY // not yet ^
        lda #%11000000  // sprite 6 & 7 Y > 255 so set bit 9
        sta $d010       // and store it

        lda #$FF
        sta $d015       // enable all sprites

        jsr PutCharInSprite7

forever:
		//wait for near screen bottom edge
		lda #$e0					
		cmp $d012
		bcs *-3


//inc $d020
        jsr scrollSprites
        jsr bounceIt
//dec $d020

    waitTop:
        lda $d011
        bpl waitTop

        ldx sinPosition
        lda sinBase,x
        //clc
        //adc #1
    waitForSpriteTop:
        cmp $d012
        bcs waitForSpriteTop
        tax
        lda spriteColour
        sta $d021
        txa
        clc
        adc #12
    waitForSpriteBottom:
        cmp $d012
        bcs waitForSpriteBottom 
        lda #0
        sta $d021



        jmp forever

        rts


}

bounceIt:
{
    lda bounceOn
    beq exit
    inc sinPosition
    ldx sinPosition

    lda sinBase,x
    ldx #0 
    bounce1:  
        sta $d001,x     //set sprite Y
        inx
        inx             // move to next X,Y pair
        cpx #$10       // have we done all 8?
        bne bounce1 // not yet ^

exit:
    rts
}

PutCharInSprite7:
{
    lda #<charBase    //start at char 0
    sta charData 
    lda #>charBase
    sta charData + 1

nextcharacter:
    lda scrolltextPointer: $C0DE
    tay
    //lda scrolltext,x    // get character
    bne testforspecial1
        //sta scrolltextPointer                // put it back to the start if we hit a 0
        lda #<scrolltext
        sta scrolltextPointer
        lda #>scrolltext
        sta scrolltextPointer+1
        jmp nextcharacter
    
    
    testforspecial1:
        bpl notspecial
        cmp #192
        bcs testforspecial2
        and #1
        sta bounceOn
        inc scrolltextPointer
        bne nextcharacter
        inc scrolltextPointer+1
        jmp nextcharacter

    testforspecial2:
        and #$3f
        sta spriteColour
        inc scrolltextPointer
        bne nextcharacter
        inc scrolltextPointer+1
        jmp nextcharacter

    // colour switch goes here

    notspecial:
    
    // work out where in char set it is
    // base + character * 8
    and #%11100000  //get bits 6/7 
    clc
    rol     // 7 > C, 6 >7
    rol     // 7>0 6>C
    rol     // 7>1 6>0 put them in bits 0/1 (divide 64)
    rol
    adc charData + 1
    sta charData + 1
    //lda scrolltext,x         // get char again
    tya
    and #%00011111          // mask off bits 7/8
    asl
    asl
    asl     // * 8
    clc
    adc charData
    sta charData    // char data now poitns to the 8 bytes of this char


    // sprite 7 data is at spritebase + 64 * 7
    // put data in column 3 of sprite rows 2-9
    lda #<spriteBase + (64*7) + 9 + 2
    sta sprite7char 
    lda #>spriteBase + (64*7) + 9 + 2
    sta sprite7char + 1
    
    ldx #7
    ldy #24
copy8:
    lda  charData: $C0DE,x
    eor #$ff
    sta  sprite7char: $C0DE,y
    dey
    dey
    dey
    dex
    bpl copy8
    rts
}

scrollSprites:
{
    
    // rol spriteBase + (64*7) + 2
    // rol spriteBase + (64*7) + 1
    // rol spriteBase + (64*7) + 0
    // rol spriteBase + (64*6) + 2
    // rol spriteBase + (64*6) + 1
    // rol spriteBase + (64*6) + 0

//inc $d020
    sec
    .for (var j=4; j<12; j++)
    {
        .for (var i=8; i>0; i--)
        {
            .for (var k=3; k>0; k--)
            {
                rol spriteBase + (64*(i-1)) + (j*3) + (k-1)
            }
        }
    }
//dec $d020
    dec nextchar
    bpl exit
        inc PutCharInSprite7.scrolltextPointer
        bne skip
        inc PutCharInSprite7.scrolltextPointer+1
    skip:
    jsr PutCharInSprite7
    lda #7
    sta nextchar
exit:    
    rts
}
        
ClearScreen: {
		lda #$20
		ldx #$00
	!:
		sta $0400, x
		sta $0500, x
		sta $0600, x
		sta $0700, x

		sta $d800, x
		sta $d900, x
		sta $da00, x
		sta $db00, x
		inx
		bne !-
		rts
}

scrolltext:     // chars above 127 trigger effects on /off  128/9 = bounce off/on, 192 + x = colour, 224/5 = multicolour on/oof
		.text "OldSkoolCoder gave us homework. A text scroller he said... "
        .byte 129
        .text " We thought it would be fun to add extra things."
        .byte 128
        .text" We can turn it on and off at will."
        .byte 129
        .text " We can change colour. Lets try"
        .byte 192+2
        .text " RED(Really? you call this RED), or maybe we want"
        .byte 192+7
        .text " YELLOW. I suppose some of you might like"
        .byte 192+14
        .text " Purple? "
        .text "Anyway, I want to do some other stuff but its work in progress and i might not have time so ..."
        .byte 128
        .text "                      "
        .byte 192+1
        .byte 0 // text terminator

* = charBase "Charset"
CharSet:

.import binary "464charset-00-7F.bin"   // charset at $3000 - $33ff

CharSetEnd:
.label charlength = CharSetEnd - CharSet


* = $3400 "SinTable"  // sin data starting at line 100  $3400-$34ff
SinTable: 

.import binary "sintab.bin"

SinTableEnd:


* = spriteBase "Sprites"  // 8 sprites $3800-$39ff
SpriteStart: 

.fill 64*8,$ff // all trans for now

SpriteEnd:
.label spriteLen = SpriteEnd - SpriteStart


