program hello
    implicit none
    !READ(*,*,IOSTAT=io)  x
    real(8), parameter :: half_lattice_parameter = 2.86
    integer :: na, lines, rawlines, i, j, addit
    !real(8) :: testreal
    real(8), allocatable, dimension(:,:) :: ux, uy, uz, rx, ry, rz, exx, exy, eyy, ezz, exz, ezy
    character( LEN = 47) :: line_count_command
    character( LEN = 2) :: filenumber
    character( LEN = 28) :: filenamex
    character( LEN = 28) :: filenamey
    character( LEN = 28) :: filenamez
    character( LEN = 34) :: filename_united
    character( LEN = 28) :: fmt01


    if(command_argument_count().ne.1)then
        print*, 'Error, two digits are required as command-line arguments, stopping'
        stop
    endif
    print *, repeat(":" // repeat(" ",14),8)
    call execute_command_line("rm lines.txt", wait = .true.)

    call get_command_argument(1, filenumber)
    !filenumber = "15"
!    if ( ( ichar(filenumber(1:1)) .lt. 0) .or. ( ichar(filenumber(1:1)) .gt. 99) ) then
!        print*, 'Error, two digits are required as command-line arguments, stopping'
!        stop
!    endif
    filenamex = "Matrix-stage" // filenumber // "-un-Ux-100.txt"
    filenamey = "Matrix-stage" // filenumber // "-un-Uy-100.txt"
    filenamez = "Matrix-stage" // filenumber // "-un-Uz-100.txt"
    filename_united = "PeratomicRxyzUxyz_st-" // filenumber // "-un-100.txt"
    line_count_command = "wc -l " // filenamex // " >> lines.txt"
    !line_count_command = "wc -l Matrix-stage" // filenumber // "-un-Ux-100.txt >> lines.txt"

    call execute_command_line (line_count_command, wait = .true.)

    addit = 0
    open (141, file = "lines.txt")
    read (141, *) rawlines
    lines = rawlines + 1
    write(fmt01,3510) '(', lines, '(1x, E11.4),1x)'

    allocate(ux(lines,lines))
    allocate(uy(lines,lines))
    allocate(uz(lines,lines))
    allocate(rx(lines,lines))
    allocate(ry(lines,lines))
    allocate(rz(lines,lines))
    allocate(exx(lines,lines))
    allocate(exy(lines,lines))
    allocate(eyy(lines,lines))
    allocate(ezz(lines,lines))
    allocate(exz(lines,lines))
    allocate(ezy(lines,lines))

    print *, "File " // filenamex // " has ", lines, "lines ; byte size sum is ", &
        sizeof(ux)  + &
        sizeof(uy)  + &
        sizeof(uz)  + &
        sizeof(rx)  + &
        sizeof(ry)  + &
        sizeof(rz)  + &
        sizeof(exx) + &
        sizeof(exy) + &
        sizeof(eyy) + &
        sizeof(ezz) + &
        sizeof(exz) + &
        sizeof(ezy)

    print *, repeat(":" // repeat(" ",14),8)
    print *, "Opening " // filenamex // " assuming that it has", lines," lines"
    open(151, file = filenamex)

    print 1009, "Reading of " // filenamex // "..."

    do j=1,lines
        read(151,fmt01) ux(1:lines,j)
    enddo

    print 1009, " Done. Corner values are : "
    print 1510, ux(1,1    ) ; print 1510, ux(lines,1    )
    print 1510, ux(1,lines) ; print 2510, ux(lines,lines)

    close(151)


    print *, repeat(":" // repeat(" ",14),8)
    print *, "Opening " // filenamey // "..."
    open(152, file = filenamey)

    print 1009, "Reading of " // filenamey // "..."

    do j=1,lines
        read(152,fmt01) uy(1:lines,j)
    enddo

    print 1009, " Done. Corner values are : "
    print 1510, uy(1,1    ) ; print 1510, uy(lines,1    )
    print 1510, uy(1,lines) ; print 2510, uy(lines,lines)

    close(152)


    print *, repeat(":" // repeat(" ",14),8)
    print *, "Opening " // filenamez // "..."
    open(153, file = filenamez)

    print 1009, "Reading of " // filenamez // "..."

    do j=1,lines
        read(153,fmt01) uz(1:lines,j)
    enddo

    print 1009, " Done. Corner values are : "
    print 1510, uz(1,1    ) ; print 1510, uz(lines,1    )
    print 1510, uz(1,lines) ; print 2510, uz(lines,lines)

    close(153)

    print *, repeat(":" // repeat(" ",14),8)
    print *, "Assigning Rx Ry Rz. to 'un'-files, assuming that z = 0 for 'un'-files "

    do j=-(lines/2),lines/2
        do i=-(lines/2),lines/2
            rx(1+i+lines/2, 1+j+lines/2) = i*half_lattice_parameter
            ry(1+i+lines/2, 1+j+lines/2) = j*half_lattice_parameter
            rz(1+i+lines/2, 1+j+lines/2) = 0d0
        enddo
    enddo

    print 1009, " Done. Rx corner values are : "
    print 1510, rx(1,1    ) ; print 1510, rx(lines,1    )
    print 1510, rx(1,lines) ; print 2510, rx(lines,lines)

    print 1009, " Done. Ry corner values are : "
    print 1510, ry(1,1    ) ; print 1510, ry(lines,1    )
    print 1510, ry(1,lines) ; print 2510, ry(lines,lines)

    print 1009, " Done. Rz corner values are : "
    print 1510, rz(1,1    ) ; print 1510, rz(lines,1    )
    print 1510, rz(1,lines) ; print 2510, rz(lines,lines)

    print *, repeat(":" // repeat(" ",14),8)

    !print 4411, exp(1.51),tiny(ux),huge(ux)
    !print 4412, 280, exp(1.51),tiny(ux),huge(ux),exp(4.51),tiny(ux),huge(ux)
    print *, "Writing Rx Ry Rz Ux Uy Uz in file " // filename_united

    open(154, file = filename_united)
    na = 1
    write(154,*) "# number x y z ux uy uz"
    do i = 1,lines
        do j = 1,lines
            write(154,4412) na,rx(i,j),ry(i,j),rz(i,j),ux(i,j),uy(i,j),uz(i,j)
            na = na + 1
        enddo
            write(154,*) " "
    enddo

    close(154)

    deallocate(ux)
    deallocate(uy)
    deallocate(uz)
    deallocate(rx)
    deallocate(ry)
    deallocate(rz)
    deallocate(exx)
    deallocate(exy)
    deallocate(eyy)
    deallocate(ezz)
    deallocate(exz)
    deallocate(ezy)


    print *, repeat(")(" // repeat(" ",13),6), filename_united
    print *, "format fmt01 is ", fmt01

    4411 format(SP,3(1x,E13.5e3)) !for
    4412 format(I7.2,SP,6(1x,E13.5e3)) !for
    3510 format(A,I3.3,A)
    1510 format(1x, E11.4,$)
    2510 format(1x, E11.4)
    2560 format(1x, E11.4, A)
    1009 format(A, $)
end program
