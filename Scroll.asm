//
//  Sorcie's Homework
//

.label charBase = $3000
.label sinBase = $3400
.label spriteBase = $3800

* = $02 "zeropage" virtual
bounceOn: .byte 0
colourBarsOn: .byte 0
sinPosition: .byte 0
nextchar: .byte 0
spriteColour: .byte 0
colourBarPointer: .byte 0

* = $0801

BasicUpstart2(Start)

Start: {
		// Set Fore/back colors and clear screen to black 
        // we can also set up some inital variables along the way while we have
        // the correct number in a
		lda #$00
        sta bounceOn
        sta colourBarsOn
        sta sinPosition
        sta colourBarPointer
		sta $d020
		sta $d021
        lda #1              
        sta spriteColour    //start sprite colour as white (really background since sprites are transparent)

        lda #<scrolltext
        sta PutCharInSprite7.scrolltextPointer
        lda #>scrolltext
        sta PutCharInSprite7.scrolltextPointer+1
		jsr ClearScreen

        ldx #7
        stx nextchar    // while we have 7 may as well initialise the scroller
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
        lda sinBase     // sprite Y start at row 128 (see sin table)
        sta $d001,x     //set sprite Y
        tya             //get our save X position back
        clc
        adc #48        // move sprite X along 48 pixels (double width)
        inx
        inx             // move to next sprite
        cpx #$10        // have we done all 8?
        bne setSpriteXY // not yet
        lda #%11000000  // sprite 6 & 7 need bit 9 setting because they are on rigth side of screen (X > 255)
        sta $d010

        lda #$FF
        sta $d015       //turn all the sprites on

        jsr PutCharInSprite7    //put first char of our text into sprite 7

forever:
//inc $d020
    topbit:
		//wait for some point lower on the screen than we ever put sprites
        //lda $d011   //wait until we are in top part of screen
        //bpl topbit
inc $d020		
        lda #200
    wait4Line240:					
		cmp $d012
		bne wait4Line240 
//inc $d020
        // do all the hard bits while we have loads of time
inc $d020
        jsr scrollSprites   // move everything left a bit
        jsr bounceIt        // do the bouncy thing if enabled
dec $d020
dec $d020
        //inc $d020
    wait4Line256:         // wait until we are at top of screen
        lda $d011
        bpl wait4Line256
    wait4ScreenTop:
        lda $d011
        bmi wait4ScreenTop
        //dec $d020

        lda colourBarsOn        
        beq justOneColour

    // do colour bars here

        ldx sinPosition
        lda sinBase,x           //get current 'top of sprites'
        //clc
        //adc #1                  //and point to next line down
        sta lineCounter
        ldy #8                // set number of bars (each is 2 lines)
        ldx colourBarPointer    // and get current position in colourlist
    waitForSpriteTop:
        cmp $d012
        bne waitForSpriteTop
    waitAnotherLine:
        cmp $d012
        beq waitAnotherLine


    nextBar:   
        lda colourList,x
        sta $d021               //set colour
        sta $d027
        inc lineCounter
        inc lineCounter
        lda lineCounter: #0
    wait4nextline:
        cmp $d012
        bne wait4nextline
        inx
        dey
        bne nextBar
        lda #0
        sta $d021
        sta $d027

        inc colourBarPointer
        lda colourBarPointer
        cmp #20
        bne NotAtEnd
        lda #0
        sta colourBarPointer
    NotAtEnd:
        jmp forever

    justOneColour:
        jsr SetSpriteColour
        jmp forever         //keep doing it again and again until user gets bored and closes vice64
        rts                 //obligatory rts because you never know
}

SetSpriteColour:
{
        ldx sinPosition
        lda sinBase,x       // find which line our sprites start on
    waitForSpriteTop:   
        cmp $d012           // and wait until we get there
        bcs waitForSpriteTop
        tax                 //remember which line we started on
        lda spriteColour
        sta $d021           //turn background colour on (sprites are transparent)
        txa                 //get start line back
        clc
        adc #15             // not realy bottom of sprite but we only use 8 lines so turn colour back off before the bottom
    waitForSpriteBottom:
        cmp $d012
        bcs waitForSpriteBottom 
        lda #0
        sta $d021           //turn background colour off
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
    inx             // move to next sprite
    cpx #$10        // have we done all 8?
    bne bounce1     // not yet ^
exit:
    rts
}

PutCharInSprite7:
{
    lda #<charBase    //start at char 0
    sta charData 
    lda #>charBase
    sta charData + 1
getCharacter:
    lda scrolltextPointer: $C0DE
    tay                     // save the characterr
    inc scrolltextPointer   //and move pointer to next character in the text
    bne notPageBoundary
    inc scrolltextPointer+1
notPageBoundary:
    tya                     //restore character to a
    bne testforspecial1     //are we at the end of the text
    lda #<scrolltext        //yep lets go back to the beginning
    sta scrolltextPointer
    lda #>scrolltext
    sta scrolltextPointer+1
    jmp getCharacter        //and get the next(first!) character
testforspecial1:
    cmp #128
    bcc notspecial          //bit 7 not set so its just a character
    cmp #192                //test for bounce control    
    bcs testforspecial2 
    and #1
    sta bounceOn
    jmp getCharacter
testforspecial2:
    cmp #224                //test for change colour
    bcs colourbars
    and #$3f
    sta spriteColour
    jmp getCharacter
colourbars:                 //if we get here then its only option left - turn colour bars on/off
    and #1
    sta colourBarsOn
    jmp getCharacter

notspecial:
    // work out where in char set it is
    // base + character * 8
//    tay             //save for later
    and #%11100000  //get bits 5/6/7  to work out which page of font data we are on
    clc
    rol     
    rol     
    rol
    rol
    adc charData + 1
    sta charData + 1
    tya                 // get original number back
    and #%00011111      // mask off bits 5/6/7 - we already dealt with them
    asl
    asl
    asl                 // * 8 for offset from start of page to this characters data
    clc
    adc charData
    sta charData        // char data now points to the 8 bytes of this char

    // sprite 7 data is at spritebase + 64 * 7
    // put data in column 3 of sprite rows 4-11  (right hand side of sprite)
    // 3 bytes per line so +9 to point to row 4 . + 2 to point to column 3
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
    // generate a whole bunch of rotate commands so we can shift bits left in the sprite data
    //can probably do this shorter but we have loads of RAM and this is quickest
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
    dec nextchar    //count the pixels
    bpl exit        //not moved 8 yet so get out of here
    lda #7          //reset pixel counter
    sta nextchar
    jsr PutCharInSprite7    //put next character into sprite7
exit:    
    rts
}
        
ClearScreen: {
		lda #$20
		ldx #$00
	loop:
		sta $0400, x
		sta $0500, x
		sta $0600, x
		sta $0700, x

		sta $d800, x
		sta $d900, x
		sta $da00, x
		sta $db00, x
		inx
		bne loop
		rts
}

scrolltext:     // chars above 127 trigger effects on /off  128/9 = bounce off/on, 192 + x = colour, 224/5 = colour bars on/off
		// .text "co"
        // .byte 192 + 6
        // .text "lourbar I want to do some other stuff but its work in progress and i might not have "
        // .byte 225
        // .text "lots of text with colourbar effect if i did this correctly"
        // .byte 129
        // .text "and hopefully it still works when we add bounce                                   "
        
        .text "OldSkoolCoder gave us homework. A text scroller he said... "
        .byte 129
        .text " We thought it would be fun to add extra things."
        .byte 128
        .text" We can turn bounce on and off."
        .byte 129
        .text " We can change colour. Lets try"
        .byte 192+2
        .text " RED(Really? you call this RED), or maybe we want"
        .byte 192+7
        .text " YELLOW. I've been told some people like the colour"
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

* = $3400 "SinTable"  // sin data starting at line 100  $3400-$34ff
SinTable: 
.import binary "sintab.bin"

colourList:
        .byte 11,11,12,12,15,15,12,12,11,11,12,12,15,15,12,12,11,11,12,12,15,15,12,12,11,11,12,12,15,15,12,12,11,11,12,12,15,15,12,12
        .byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6

* = spriteBase "Sprites"  // 8 sprites $3800-$39ff
SpriteStart: 
.fill 64*8,$ff // fill sprite data all pixels on
