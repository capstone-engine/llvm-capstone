# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | FileCheck %s

# source file to test assembly using various styles of
# CP0 (w/ non-zero select code) register names.

        .text
text_label:
        mfc0        $a0, $index         # CHECK: mfc0    $a0, $index        # encoding: [0x80,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 80 20 30 00        mfc0 $a0, $index
        mfc0        $a0, $mvpcontrol    # CHECK: mfc0    $a0, $mvpcontrol        # encoding: [0x80,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 80 20 30 08        mfc0 $a0, $mvpcontrol
        mfc0        $a0, $mvpconf0      # CHECK: mfc0    $a0, $mvpconf0        # encoding: [0x80,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 80 20 30 10        mfc0 $a0, $mvpconf0
        mfc0        $a0, $mvpconf1      # CHECK: mfc0    $a0, $mvpconf1        # encoding: [0x80,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 80 20 30 18        mfc0 $a0, $mvpconf1
        mfc0        $a0, $vpcontrol     # CHECK: mfc0    $a0, $vpcontrol        # encoding: [0x80,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 80 20 30 20        mfc0 $a0, $vpcontrol
        mfc0        $a0, $random        # CHECK: mfc0    $a0, $random        # encoding: [0x81,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 81 20 30 00        mfc0 $a0, $random
        mfc0        $a0, $vpecontrol    # CHECK: mfc0    $a0, $vpecontrol        # encoding: [0x81,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 81 20 30 08        mfc0 $a0, $vpecontrol
        mfc0        $a0, $vpeconf0      # CHECK: mfc0    $a0, $vpeconf0        # encoding: [0x81,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 81 20 30 10        mfc0 $a0, $vpeconf0
        mfc0        $a0, $vpeconf1      # CHECK: mfc0    $a0, $vpeconf1        # encoding: [0x81,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 81 20 30 18        mfc0 $a0, $vpeconf1
        mfc0        $a0, $yqmask        # CHECK: mfc0    $a0, $yqmask        # encoding: [0x81,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 81 20 30 20        mfc0 $a0, $yqmask
        mfc0        $a0, $vpeschedule   # CHECK: mfc0    $a0, $vpeschedule        # encoding: [0x81,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 81 20 30 28        mfc0 $a0, $vpeschedule
        mfc0        $a0, $vpeschefback  # CHECK: mfc0    $a0, $vpeschefback        # encoding: [0x81,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 81 20 30 30        mfc0 $a0, $vpeschefback
        mfc0        $a0, $vpeopt        # CHECK: mfc0    $a0, $vpeopt        # encoding: [0x81,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 81 20 30 38        mfc0 $a0, $vpeopt
        mfc0        $a0, $tcstatus      # CHECK: mfc0    $a0, $tcstatus        # encoding: [0x82,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 82 20 30 08        mfc0 $a0, $tcstatus
        mfc0        $a0, $tcbind        # CHECK: mfc0    $a0, $tcbind        # encoding: [0x82,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 82 20 30 10        mfc0 $a0, $tcbind
        mfc0        $a0, $tcrestart     # CHECK: mfc0    $a0, $tcrestart        # encoding: [0x82,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 82 20 30 18        mfc0 $a0, $tcrestart
        mfc0        $a0, $tchalt        # CHECK: mfc0    $a0, $tchalt        # encoding: [0x82,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 82 20 30 20        mfc0 $a0, $tchalt
        mfc0        $a0, $tccontext     # CHECK: mfc0    $a0, $tccontext        # encoding: [0x82,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 82 20 30 28        mfc0 $a0, $tccontext
        mfc0        $a0, $tcschedule    # CHECK: mfc0    $a0, $tcschedule        # encoding: [0x82,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 82 20 30 30        mfc0 $a0, $tcschedule
        mfc0        $a0, $tcschefback   # CHECK: mfc0    $a0, $tcschefback        # encoding: [0x82,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 82 20 30 38        mfc0 $a0, $tcschefback
        mfc0        $a0, $globalnumber  # CHECK: mfc0    $a0, $globalnumber        # encoding: [0x83,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 83 20 30 08        mfc0 $a0, $globalnumber
        mfc0        $a0, $tcopt         # CHECK: mfc0    $a0, $tcopt        # encoding: [0x83,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 83 20 30 38        mfc0 $a0, $tcopt
        mfc0        $a0, $contextconfig # CHECK: mfc0    $a0, $contextconfig        # encoding: [0x84,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 84 20 30 08        mfc0 $a0, $contextconfig
        mfc0        $a0, $userlocal     # CHECK: mfc0    $a0, $userlocal        # encoding: [0x84,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 84 20 30 10        mfc0 $a0, $userlocal
        mfc0        $a0, $xcontextconfig    # CHECK: mfc0    $a0, $xcontextconfig        # encoding: [0x84,0x20,0x30,0x18]
                                            # DISAS: {{.*}}: 84 20 30 18        mfc0 $a0, $xcontextconfig
        mfc0        $a0, $debugcontextid    # CHECK: mfc0    $a0, $debugcontextid        # encoding: [0x84,0x20,0x30,0x20]
                                            # DISAS: {{.*}}: 84 20 30 20        mfc0 $a0, $debugcontextid
        mfc0        $a0, $memorymapid   # CHECK: mfc0    $a0, $memorymapid        # encoding: [0x84,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 84 20 30 28        mfc0 $a0, $memorymapid
        mfc0        $a0, $pagemask      # CHECK: mfc0    $a0, $pagemask        # encoding: [0x85,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 85 20 30 00        mfc0 $a0, $pagemask
        mfc0        $a0, $pagegrain     # CHECK: mfc0    $a0, $pagegrain        # encoding: [0x85,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 85 20 30 08        mfc0 $a0, $pagegrain
        mfc0        $a0, $segctl0       # CHECK: mfc0    $a0, $segctl0        # encoding: [0x85,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 85 20 30 10        mfc0 $a0, $segctl0
        mfc0        $a0, $segctl1       # CHECK: mfc0    $a0, $segctl1        # encoding: [0x85,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 85 20 30 18        mfc0 $a0, $segctl1
        mfc0        $a0, $segctl2       # CHECK: mfc0    $a0, $segctl2        # encoding: [0x85,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 85 20 30 20        mfc0 $a0, $segctl2
        mfc0        $a0, $pwbase        # CHECK: mfc0    $a0, $pwbase        # encoding: [0x85,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 85 20 30 28        mfc0 $a0, $pwbase
        mfc0        $a0, $pwfield       # CHECK: mfc0    $a0, $pwfield        # encoding: [0x85,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 85 20 30 30        mfc0 $a0, $pwfield
        mfc0        $a0, $pwsize        # CHECK: mfc0    $a0, $pwsize        # encoding: [0x85,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 85 20 30 38        mfc0 $a0, $pwsize
        mfc0        $a0, $wired         # CHECK: mfc0    $a0, $wired        # encoding: [0x86,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 86 20 30 00        mfc0 $a0, $wired
        mfc0        $a0, $srsconf0      # CHECK: mfc0    $a0, $srsconf0        # encoding: [0x86,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 86 20 30 08        mfc0 $a0, $srsconf0
        mfc0        $a0, $srsconf1      # CHECK: mfc0    $a0, $srsconf1        # encoding: [0x86,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 86 20 30 10        mfc0 $a0, $srsconf1
        mfc0        $a0, $srsconf2      # CHECK: mfc0    $a0, $srsconf2        # encoding: [0x86,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 86 20 30 18        mfc0 $a0, $srsconf2
        mfc0        $a0, $srsconf3      # CHECK: mfc0    $a0, $srsconf3        # encoding: [0x86,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 86 20 30 20        mfc0 $a0, $srsconf3
        mfc0        $a0, $srsconf4      # CHECK: mfc0    $a0, $srsconf4        # encoding: [0x86,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 86 20 30 28        mfc0 $a0, $srsconf4
        mfc0        $a0, $pwctl         # CHECK: mfc0    $a0, $pwctl        # encoding: [0x86,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 86 20 30 30        mfc0 $a0, $pwctl
        mfc0        $a0, $hwrena        # CHECK: mfc0    $a0, $hwrena        # encoding: [0x87,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 87 20 30 00        mfc0 $a0, $hwrena
        mfc0        $a0, $badvaddr      # CHECK: mfc0    $a0, $badvaddr        # encoding: [0x88,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 88 20 30 00        mfc0 $a0, $badvaddr
        mfc0        $a0, $badinst       # CHECK: mfc0    $a0, $badinst        # encoding: [0x88,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 88 20 30 08        mfc0 $a0, $badinst
        mfc0        $a0, $badinstrp     # CHECK: mfc0    $a0, $badinstrp        # encoding: [0x88,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 88 20 30 10        mfc0 $a0, $badinstrp
        mfc0        $a0, $badinstrx     # CHECK: mfc0    $a0, $badinstrx        # encoding: [0x88,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 88 20 30 18        mfc0 $a0, $badinstrx
        mfc0        $a0, $count         # CHECK: mfc0    $a0, $count        # encoding: [0x89,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 89 20 30 00        mfc0 $a0, $count
        mfc0        $a0, $entryhi       # CHECK: mfc0    $a0, $entryhi        # encoding: [0x8a,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8a 20 30 00        mfc0 $a0, $entryhi
        mfc0        $a0, $guestctl1     # CHECK: mfc0    $a0, $guestctl1        # encoding: [0x8a,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 8a 20 30 20        mfc0 $a0, $guestctl1
        mfc0        $a0, $guestctl2     # CHECK: mfc0    $a0, $guestctl2        # encoding: [0x8a,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 8a 20 30 28        mfc0 $a0, $guestctl2
        mfc0        $a0, $guestctl3     # CHECK: mfc0    $a0, $guestctl3        # encoding: [0x8a,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 8a 20 30 30        mfc0 $a0, $guestctl3
        mfc0        $a0, $compare       # CHECK: mfc0    $a0, $compare        # encoding: [0x8b,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8b 20 30 00        mfc0 $a0, $compare
        mfc0        $a0, $guestctl0ext  # CHECK: mfc0    $a0, $guestctl0ext        # encoding: [0x8b,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 8b 20 30 20        mfc0 $a0, $guestctl0ext
        mfc0        $a0, $status        # CHECK: mfc0    $a0, $status        # encoding: [0x8c,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8c 20 30 00        mfc0 $a0, $status
        mfc0        $a0, $intctl        # CHECK: mfc0    $a0, $intctl        # encoding: [0x8c,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 8c 20 30 08        mfc0 $a0, $intctl
        mfc0        $a0, $srsctl        # CHECK: mfc0    $a0, $srsctl        # encoding: [0x8c,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 8c 20 30 10        mfc0 $a0, $srsctl
        mfc0        $a0, $srsmap        # CHECK: mfc0    $a0, $srsmap        # encoding: [0x8c,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 8c 20 30 18        mfc0 $a0, $srsmap
        mfc0        $a0, $view_ipl      # CHECK: mfc0    $a0, $view_ipl    # encoding: [0x8c,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 8c 20 30 20        mfc0 $a0, $view_ipl
        mfc0        $a0, $srsmap2       # CHECK: mfc0    $a0, $srsmap2        # encoding: [0x8c,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 8c 20 30 28        mfc0 $a0, $srsmap2
        mfc0        $a0, $guestctl0     # CHECK: mfc0    $a0, $guestctl0        # encoding: [0x8c,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 8c 20 30 30        mfc0 $a0, $guestctl0
        mfc0        $a0, $gtoffset      # CHECK: mfc0    $a0, $gtoffset        # encoding: [0x8c,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 8c 20 30 38        mfc0 $a0, $gtoffset
        mfc0        $a0, $cause         # CHECK: mfc0    $a0, $cause        # encoding: [0x8d,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8d 20 30 00        mfc0 $a0, $cause
        mfc0        $a0, $view_ripl     # CHECK: mfc0    $a0, $view_ripl   # encoding: [0x8d,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 8d 20 30 20        mfc0 $a0, $view_ripl
        mfc0        $a0, $nestedexc     # CHECK: mfc0    $a0, $nestedexc        # encoding: [0x8d,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 8d 20 30 28        mfc0 $a0, $nestedexc
        mfc0        $a0, $epc           # CHECK: mfc0    $a0, $epc        # encoding: [0x8e,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8e 20 30 00        mfc0 $a0, $epc
        mfc0        $a0, $nestedepc     # CHECK: mfc0    $a0, $nestedepc        # encoding: [0x8e,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 8e 20 30 10        mfc0 $a0, $nestedepc
        mfc0        $a0, $prid          # CHECK: mfc0    $a0, $prid        # encoding: [0x8f,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 8f 20 30 00        mfc0 $a0, $prid
        mfc0        $a0, $ebase         # CHECK: mfc0    $a0, $ebase        # encoding: [0x8f,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 8f 20 30 08        mfc0 $a0, $ebase
        mfc0        $a0, $cdmmbase      # CHECK: mfc0    $a0, $cdmmbase        # encoding: [0x8f,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 8f 20 30 10        mfc0 $a0, $cdmmbase
        mfc0        $a0, $cmgcrbase     # CHECK: mfc0    $a0, $cmgcrbase        # encoding: [0x8f,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 8f 20 30 18        mfc0 $a0, $cmgcrbase
        mfc0        $a0, $bevva         # CHECK: mfc0    $a0, $bevva        # encoding: [0x8f,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 8f 20 30 20        mfc0 $a0, $bevva
        mfc0        $a0, $config        # CHECK: mfc0    $a0, $config        # encoding: [0x90,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 90 20 30 00        mfc0 $a0, $config
        mfc0        $a0, $config1       # CHECK: mfc0    $a0, $config1        # encoding: [0x90,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 90 20 30 08        mfc0 $a0, $config1
        mfc0        $a0, $config2       # CHECK: mfc0    $a0, $config2        # encoding: [0x90,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 90 20 30 10        mfc0 $a0, $config2
        mfc0        $a0, $config3       # CHECK: mfc0    $a0, $config3        # encoding: [0x90,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 90 20 30 18        mfc0 $a0, $config3
        mfc0        $a0, $config4       # CHECK: mfc0    $a0, $config4        # encoding: [0x90,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 90 20 30 20        mfc0 $a0, $config4
        mfc0        $a0, $config5       # CHECK: mfc0    $a0, $config5        # encoding: [0x90,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 90 20 30 28        mfc0 $a0, $config5
        mfc0        $a0, $lladdr        # CHECK: mfc0    $a0, $lladdr        # encoding: [0x91,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 91 20 30 00        mfc0 $a0, $lladdr
        mfc0        $a0, $maar          # CHECK: mfc0    $a0, $maar        # encoding: [0x91,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 91 20 30 08        mfc0 $a0, $maar
        mfc0        $a0, $maari         # CHECK: mfc0    $a0, $maari        # encoding: [0x91,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 91 20 30 10        mfc0 $a0, $maari
        mfc0        $a0, $watchlo0      # CHECK: mfc0    $a0, $watchlo0        # encoding: [0x92,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 92 20 30 00        mfc0 $a0, $watchlo0
        mfc0        $a0, $watchlo1      # CHECK: mfc0    $a0, $watchlo1        # encoding: [0x92,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 92 20 30 08        mfc0 $a0, $watchlo1
        mfc0        $a0, $watchlo2      # CHECK: mfc0    $a0, $watchlo2        # encoding: [0x92,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 92 20 30 10        mfc0 $a0, $watchlo2
        mfc0        $a0, $watchlo3      # CHECK: mfc0    $a0, $watchlo3        # encoding: [0x92,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 92 20 30 18        mfc0 $a0, $watchlo3
        mfc0        $a0, $watchlo4      # CHECK: mfc0    $a0, $watchlo4        # encoding: [0x92,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 92 20 30 20        mfc0 $a0, $watchlo4
        mfc0        $a0, $watchlo5      # CHECK: mfc0    $a0, $watchlo5        # encoding: [0x92,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 92 20 30 28        mfc0 $a0, $watchlo5
        mfc0        $a0, $watchlo6      # CHECK: mfc0    $a0, $watchlo6        # encoding: [0x92,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 92 20 30 30        mfc0 $a0, $watchlo6
        mfc0        $a0, $watchlo7      # CHECK: mfc0    $a0, $watchlo7        # encoding: [0x92,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 92 20 30 38        mfc0 $a0, $watchlo7
        mfc0        $a0, $watchlo8      # CHECK: mfc0    $a0, $watchlo8        # encoding: [0x92,0x20,0x30,0x40]
                                        # DISAS: {{.*}}: 92 20 30 40        mfc0 $a0, $watchlo8
        mfc0        $a0, $watchlo9      # CHECK: mfc0    $a0, $watchlo9        # encoding: [0x92,0x20,0x30,0x48]
                                        # DISAS: {{.*}}: 92 20 30 48        mfc0 $a0, $watchlo9
        mfc0        $a0, $watchlo10     # CHECK: mfc0    $a0, $watchlo10        # encoding: [0x92,0x20,0x30,0x50]
                                        # DISAS: {{.*}}: 92 20 30 50        mfc0 $a0, $watchlo10
        mfc0        $a0, $watchlo11     # CHECK: mfc0    $a0, $watchlo11        # encoding: [0x92,0x20,0x30,0x58]
                                        # DISAS: {{.*}}: 92 20 30 58        mfc0 $a0, $watchlo11
        mfc0        $a0, $watchlo12     # CHECK: mfc0    $a0, $watchlo12        # encoding: [0x92,0x20,0x30,0x60]
                                        # DISAS: {{.*}}: 92 20 30 60        mfc0 $a0, $watchlo12
        mfc0        $a0, $watchlo13     # CHECK: mfc0    $a0, $watchlo13        # encoding: [0x92,0x20,0x30,0x68]
                                        # DISAS: {{.*}}: 92 20 30 68        mfc0 $a0, $watchlo13
        mfc0        $a0, $watchlo14     # CHECK: mfc0    $a0, $watchlo14        # encoding: [0x92,0x20,0x30,0x70]
                                        # DISAS: {{.*}}: 92 20 30 70        mfc0 $a0, $watchlo14
        mfc0        $a0, $watchlo15     # CHECK: mfc0    $a0, $watchlo15        # encoding: [0x92,0x20,0x30,0x78]
                                        # DISAS: {{.*}}: 92 20 30 78        mfc0 $a0, $watchlo15
        mfc0        $a0, $watchhi0      # CHECK: mfc0    $a0, $watchhi0        # encoding: [0x93,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 93 20 30 00        mfc0 $a0, $watchhi0
        mfc0        $a0, $watchhi1      # CHECK: mfc0    $a0, $watchhi1        # encoding: [0x93,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 93 20 30 08        mfc0 $a0, $watchhi1
        mfc0        $a0, $watchhi2      # CHECK: mfc0    $a0, $watchhi2        # encoding: [0x93,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 93 20 30 10        mfc0 $a0, $watchhi2
        mfc0        $a0, $watchhi3      # CHECK: mfc0    $a0, $watchhi3        # encoding: [0x93,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 93 20 30 18        mfc0 $a0, $watchhi3
        mfc0        $a0, $watchhi4      # CHECK: mfc0    $a0, $watchhi4        # encoding: [0x93,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 93 20 30 20        mfc0 $a0, $watchhi4
        mfc0        $a0, $watchhi5      # CHECK: mfc0    $a0, $watchhi5        # encoding: [0x93,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 93 20 30 28        mfc0 $a0, $watchhi5
        mfc0        $a0, $watchhi6      # CHECK: mfc0    $a0, $watchhi6        # encoding: [0x93,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 93 20 30 30        mfc0 $a0, $watchhi6
        mfc0        $a0, $watchhi7      # CHECK: mfc0    $a0, $watchhi7        # encoding: [0x93,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 93 20 30 38        mfc0 $a0, $watchhi7
        mfc0        $a0, $watchhi8      # CHECK: mfc0    $a0, $watchhi8        # encoding: [0x93,0x20,0x30,0x40]
                                        # DISAS: {{.*}}: 93 20 30 40        mfc0 $a0, $watchhi8
        mfc0        $a0, $watchhi9      # CHECK: mfc0    $a0, $watchhi9        # encoding: [0x93,0x20,0x30,0x48]
                                        # DISAS: {{.*}}: 93 20 30 48        mfc0 $a0, $watchhi9
        mfc0        $a0, $watchhi10     # CHECK: mfc0    $a0, $watchhi10        # encoding: [0x93,0x20,0x30,0x50]
                                        # DISAS: {{.*}}: 93 20 30 50        mfc0 $a0, $watchhi10
        mfc0        $a0, $watchhi11     # CHECK: mfc0    $a0, $watchhi11        # encoding: [0x93,0x20,0x30,0x58]
                                        # DISAS: {{.*}}: 93 20 30 58        mfc0 $a0, $watchhi11
        mfc0        $a0, $watchhi12     # CHECK: mfc0    $a0, $watchhi12        # encoding: [0x93,0x20,0x30,0x60]
                                        # DISAS: {{.*}}: 93 20 30 60        mfc0 $a0, $watchhi12
        mfc0        $a0, $watchhi13     # CHECK: mfc0    $a0, $watchhi13        # encoding: [0x93,0x20,0x30,0x68]
                                        # DISAS: {{.*}}: 93 20 30 68        mfc0 $a0, $watchhi13
        mfc0        $a0, $watchhi14     # CHECK: mfc0    $a0, $watchhi14        # encoding: [0x93,0x20,0x30,0x70]
                                        # DISAS: {{.*}}: 93 20 30 70        mfc0 $a0, $watchhi14
        mfc0        $a0, $watchhi15     # CHECK: mfc0    $a0, $watchhi15        # encoding: [0x93,0x20,0x30,0x78]
                                        # DISAS: {{.*}}: 93 20 30 78        mfc0 $a0, $watchhi15
        mfc0        $a0, $xcontext      # CHECK: mfc0    $a0, $xcontext        # encoding: [0x94,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 94 20 30 00        mfc0 $a0, $xcontext
        mfc0        $a0, $debug         # CHECK: mfc0    $a0, $debug        # encoding: [0x97,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 97 20 30 00        mfc0 $a0, $debug
        mfc0        $a0, $tracecontrol  # CHECK: mfc0    $a0, $tracecontrol        # encoding: [0x97,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 97 20 30 08        mfc0 $a0, $tracecontrol
        mfc0        $a0, $tracecontrol2 # CHECK: mfc0    $a0, $tracecontrol2        # encoding: [0x97,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 97 20 30 10        mfc0 $a0, $tracecontrol2
        mfc0        $a0, $usertracedata1   # CHECK: mfc0    $a0, $usertracedata1        # encoding: [0x97,0x20,0x30,0x18]
                                           # DISAS: {{.*}}: 97 20 30 18        mfc0 $a0, $usertracedata1
        mfc0        $a0, $traceibpc     # CHECK: mfc0    $a0, $traceibpc        # encoding: [0x97,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 97 20 30 20        mfc0 $a0, $traceibpc
        mfc0        $a0, $tracedbpc     # CHECK: mfc0    $a0, $tracedbpc        # encoding: [0x97,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 97 20 30 28        mfc0 $a0, $tracedbpc
        mfc0        $a0, $debug2        # CHECK: mfc0    $a0, $debug2        # encoding: [0x97,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 97 20 30 30        mfc0 $a0, $debug2
        mfc0        $a0, $depc          # CHECK: mfc0    $a0, $depc        # encoding: [0x98,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 98 20 30 00        mfc0 $a0, $depc
        mfc0        $a0, $tracecontrol3 # CHECK: mfc0    $a0, $tracecontrol3        # encoding: [0x98,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 98 20 30 10        mfc0 $a0, $tracecontrol3
        mfc0        $a0, $usertracedata2    # CHECK: mfc0    $a0, $usertracedata2        # encoding: [0x98,0x20,0x30,0x18]
                                            # DISAS: {{.*}}: 98 20 30 18        mfc0 $a0, $usertracedata2
        mfc0        $a0, $perfcnt0      # CHECK: mfc0    $a0, $perfcnt0        # encoding: [0x99,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 99 20 30 08        mfc0 $a0, $perfcnt0
        mfc0        $a0, $perfctl0      # CHECK: mfc0    $a0, $perfctl0        # encoding: [0x99,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 99 20 30 00        mfc0 $a0, $perfctl0
        mfc0        $a0, $perfcnt1      # CHECK: mfc0    $a0, $perfcnt1        # encoding: [0x99,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 99 20 30 18        mfc0 $a0, $perfcnt1
        mfc0        $a0, $perfctl1      # CHECK: mfc0    $a0, $perfctl1        # encoding: [0x99,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 99 20 30 10        mfc0 $a0, $perfctl1
        mfc0        $a0, $perfcnt2      # CHECK: mfc0    $a0, $perfcnt2        # encoding: [0x99,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 99 20 30 28        mfc0 $a0, $perfcnt2
        mfc0        $a0, $perfctl2      # CHECK: mfc0    $a0, $perfctl2        # encoding: [0x99,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 99 20 30 20        mfc0 $a0, $perfctl2
        mfc0        $a0, $perfcnt3      # CHECK: mfc0    $a0, $perfcnt3        # encoding: [0x99,0x20,0x30,0x38]
                                        # DISAS: {{.*}}: 99 20 30 38        mfc0 $a0, $perfcnt3
        mfc0        $a0, $perfctl3      # CHECK: mfc0    $a0, $perfctl3        # encoding: [0x99,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 99 20 30 30        mfc0 $a0, $perfctl3
        mfc0        $a0, $perfcnt4      # CHECK: mfc0    $a0, $perfcnt4        # encoding: [0x99,0x20,0x30,0x48]
                                        # DISAS: {{.*}}: 99 20 30 48        mfc0 $a0, $perfcnt4
        mfc0        $a0, $perfctl4      # CHECK: mfc0    $a0, $perfctl4        # encoding: [0x99,0x20,0x30,0x40]
                                        # DISAS: {{.*}}: 99 20 30 40        mfc0 $a0, $perfctl4
        mfc0        $a0, $perfcnt5      # CHECK: mfc0    $a0, $perfcnt5        # encoding: [0x99,0x20,0x30,0x58]
                                        # DISAS: {{.*}}: 99 20 30 58        mfc0 $a0, $perfcnt5
        mfc0        $a0, $perfctl5      # CHECK: mfc0    $a0, $perfctl5        # encoding: [0x99,0x20,0x30,0x50]
                                        # DISAS: {{.*}}: 99 20 30 50        mfc0 $a0, $perfctl5
        mfc0        $a0, $perfcnt6      # CHECK: mfc0    $a0, $perfcnt6        # encoding: [0x99,0x20,0x30,0x68]
                                        # DISAS: {{.*}}: 99 20 30 68        mfc0 $a0, $perfcnt6
        mfc0        $a0, $perfctl6      # CHECK: mfc0    $a0, $perfctl6        # encoding: [0x99,0x20,0x30,0x60]
                                        # DISAS: {{.*}}: 99 20 30 60        mfc0 $a0, $perfctl6
        mfc0        $a0, $perfcnt7      # CHECK: mfc0    $a0, $perfcnt7        # encoding: [0x99,0x20,0x30,0x78]
                                        # DISAS: {{.*}}: 99 20 30 78        mfc0 $a0, $perfcnt7
        mfc0        $a0, $perfctl7      # CHECK: mfc0    $a0, $perfctl7        # encoding: [0x99,0x20,0x30,0x70]
                                        # DISAS: {{.*}}: 99 20 30 70        mfc0 $a0, $perfctl7
        mfc0        $a0, $errctl        # CHECK: mfc0    $a0, $errctl        # encoding: [0x9a,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9a 20 30 00        mfc0 $a0, $errctl
        mfc0        $a0, $cacheerr      # CHECK: mfc0    $a0, $cacheerr        # encoding: [0x9b,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9b 20 30 00        mfc0 $a0, $cacheerr
        mfc0        $a0, $itaglo        # CHECK: mfc0    $a0, $itaglo        # encoding: [0x9c,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9c 20 30 00        mfc0 $a0, $itaglo
        mfc0        $a0, $idatalo       # CHECK: mfc0    $a0, $idatalo        # encoding: [0x9c,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 9c 20 30 08        mfc0 $a0, $idatalo
        mfc0        $a0, $dtaglo        # CHECK: mfc0    $a0, $dtaglo        # encoding: [0x9c,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 9c 20 30 10        mfc0 $a0, $dtaglo
        mfc0        $a0, $ddatalo       # CHECK: mfc0    $a0, $ddatalo        # encoding: [0x9c,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 9c 20 30 18        mfc0 $a0, $ddatalo
        mfc0        $a0, $itaghi        # CHECK: mfc0    $a0, $itaghi        # encoding: [0x9d,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9d 20 30 00        mfc0 $a0, $itaghi
        mfc0        $a0, $idatahi       # CHECK: mfc0    $a0, $idatahi        # encoding: [0x9d,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 9d 20 30 08        mfc0 $a0, $idatahi
        mfc0        $a0, $dtaghi        # CHECK: mfc0    $a0, $dtaghi        # encoding: [0x9d,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 9d 20 30 10        mfc0 $a0, $dtaghi
        mfc0        $a0, $ddatahi       # CHECK: mfc0    $a0, $ddatahi        # encoding: [0x9d,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 9d 20 30 18        mfc0 $a0, $ddatahi
        mfc0        $a0, $errorepc      # CHECK: mfc0    $a0, $errorepc        # encoding: [0x9e,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9e 20 30 00        mfc0 $a0, $errorepc
        mfc0        $a0, $desave        # CHECK: mfc0    $a0, $desave        # encoding: [0x9f,0x20,0x30,0x00]
                                        # DISAS: {{.*}}: 9f 20 30 00        mfc0 $a0, $desave
        mfc0        $a0, $kscratch1     # CHECK: mfc0    $a0, $kscratch1        # encoding: [0x9f,0x20,0x30,0x10]
                                        # DISAS: {{.*}}: 9f 20 30 10        mfc0 $a0, $kscratch1
        mfc0        $a0, $kscratch2     # CHECK: mfc0    $a0, $kscratch2        # encoding: [0x9f,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 9f 20 30 18        mfc0 $a0, $kscratch2
        mfc0        $a0, $kscratch3     # CHECK: mfc0    $a0, $kscratch3        # encoding: [0x9f,0x20,0x30,0x20]
                                        # DISAS: {{.*}}: 9f 20 30 20        mfc0 $a0, $kscratch3
        mtc0        $a0, $kscratch4     # CHECK: mtc0    $a0, $kscratch4        # encoding: [0x9f,0x20,0x70,0x28]
                                        # DISAS: {{.*}}: 9f 20 70 28        mtc0 $a0, $kscratch4
        mfhc0       $a0, $kscratch5     # CHECK: mfhc0    $a0, $kscratch5        # encoding: [0x9f,0x20,0x38,0x30]
                                        # DISAS: {{.*}}: 9f 20 38 30        mfhc0 $a0, $kscratch5
        mthc0       $a0, $kscratch6     # CHECK: mthc0    $a0, $kscratch6        # encoding: [0x9f,0x20,0x78,0x38]
                                        # DISAS: {{.*}}: 9f 20 78 38        mthc0 $a0, $kscratch6
	mfc0	    $a0, $0, 1          # CHECK: mfc0    $a0, $0, 1        # encoding: [0x80,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 80 20 30 00        mfc0 $a0, $index
	mfc0	    $a0, $0, 5          # CHECK: mfc0    $a0, $0, 5        # encoding: [0x80,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 80 20 30 28        mfc0 $a0, $0, 5
	mfc0	    $a0, $16, 3         # CHECK: mfc0    $a0, $16, 3        # encoding: [0x90,0x20,0x30,0x18]
                                        # DISAS: {{.*}}: 90 20 30 18        mfc0 $a0, $config3
	mfc0	    $a0, $16, 6         # CHECK: mfc0    $a0, $16, 6        # encoding: [0x90,0x20,0x30,0x30]
                                        # DISAS: {{.*}}: 90 20 30 30        mfc0 $a0, $16, 6
	mfc0	    $a0, $31, 5         # CHECK: mfc0    $a0, $31, 5        # encoding: [0x9f,0x20,0x30,0x28]
                                        # DISAS: {{.*}}: 9f 20 70 28        mfc0 $a0, $kscratch4
	mfc0	    $a0, $31, 1         # CHECK: mfc0    $a0, $31, 1        # encoding: [0x9f,0x20,0x30,0x08]
                                        # DISAS: {{.*}}: 9f 20 30 08        mfc0 $a0, $31, 1