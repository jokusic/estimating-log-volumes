! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! Date: February 1, 2019
! Author: Jovana Kusic
! Student #: 0955683
! Summary: this program prompts the user to input various attributes of a saw log
! in order to determine its volume in both board feet and cubic metres
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
program main 
    ! Ensures no implicit variables can be used
    implicit none
    call getLOGdata() 
end program 

!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! This subroutine collects the necessary data to calculate
! the volume of a saw log in board feet and cubic metres
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine getLOGdata () 
    implicit none
    real :: DIBlarge, DIBsmall, totalLength, volume 
    integer :: kerf 
    character :: newLine
    ! Loop will continue until the user inputs a '-1'
    do 
        print*,'*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
        print*,'------LOG VOLUME ESTIMATOR------'
        print*,'*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
        print*,'       Enter -1 to exit', new_line(newLine)
        ! Requests for user to enter the diamater at the logs
        ! large and small end, the log length and KERF
        print*,'Diameter at Logs Large End (inches): ' 
        read (*,*) DIBlarge 
        if(DIBlarge == -1) exit
        print*, 'Diameter at Logs Small End (inches): ' 
        read (*,*) DIBsmall 
        if(DIBsmall == -1) exit
        print*,'Total log length (feet): ' 
        read (*,*) totalLength 
        if(totalLength == -1) exit
        print*,'KERF (1 for 1/4" or 0 for 1/8"): ' 
        read (*,*) kerf 
        if(kerf == -1) exit
        ! The program will not accept if the user enters anything
        ! other than a number

        print*,'------RESULTS------'
        call calcLOGjclark(DIBlarge, DIBsmall, totalLength, kerf, volume) 
        if(volume == 0) then
            print*, 'Error calculating volume'
        else 
            call calcLOGvolume(DIBlarge, DIBsmall, totalLength, volume) 
        end if
        print*, new_line(newLine) 
     end do
     return 
end

! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! This subroutine was written by J.E.Brickell of the u.s.forest service, 
! with adjustments made by Jovana Kusic
! to calculate board foot volume of sawlogs by the international rule.
! variables in the calling sequence are:
!       DIBlarge    = dib at log’s large end (inches) (0.0 if 1/2 inch taper)
!       DIBsmall    = log’s scaling diameter (inches)
!       totalLength = total log length (feet)
!       kerf        >0 if kerf assumption is 1/4 inch
!       kerf        <0, or = 0, if kerf assumption is 1/8 inch
!       volume      = log volume returned to the calling program
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine calcLOGjclark (DIBlarge, DIBsmall, totalLength, kerf, volume) 
    implicit none
    real, intent(in) :: DIBlarge, DIBsmall, totalLength
    integer, intent(in) :: kerf
    real, intent(out) :: volume
    real :: amountOfFeet, amountOfFeetLeftover, diameter, diameterC, diameterEX, taperRate, segmentLength, volumeOfSmallEnd
    integer :: length, I = 0
    volume= 0.0
    ! If total log length is less than four feet no board foot volume will be computed
    if(totalLength-4.0 < 0) then
        return
    end if
    ! If the log’s large end diameter is furnished to jclark a taper rate 
    ! will be computed. if dl=0 the standard assumption of 1/2 inch per 4
    ! feet of log length will be used.
    if(DIBlarge <= 0) then
        taperRate= 0.5
    else
        taperRate= 4.0 * (DIBlarge-DIBsmall)/totalLength
    end if

    ! The following loop finds out how many full 4
    ! foot segments the log contains.
    do I=1,20 
        if((totalLength-FLOAT(4*I)) < 0) exit
    end do

    length= I-1 
    segmentLength= FLOAT(4*length) 
    ! The following statement moves the scaling diameter down to the end of
    ! the 4 foot segments and increases it according to taper.
    diameter= DIBsmall+(taperRate/4.0)*(totalLength-segmentLength) 

    ! The following loop finds out how many full feet 
    ! of length are in the segment less than 4 feet long.
    do I=1,4 
        amountOfFeet= FLOAT(I) 
        if((segmentLength-totalLength+amountOfFeet) > 0) exit 
    end do

    ! The next three statements calculate log volume in the 1, 2, or 3 foot 
    ! segment at the small end of the log.
    amountOfFeetLeftover= amountOfFeet-1.0 
    diameterEX= DIBsmall+(taperRate/4.0)*(totalLength-segmentLength-amountOfFeetLeftover) 
    volumeOfSmallEnd= 0.055*amountOfFeetLeftover*diameterEX*diameterEX-0.1775*amountOfFeetLeftover*diameterEX 
 
    ! The following loop calculates volume in the portion of
    ! the log containing whole 4 foot segments. 
    do I=1,length 
        diameterC= diameter+taperRate*FLOAT(I-1) 
        volume= volume+0.22*diameterC*diameterC-0.71*diameterC 
    end do
    volume= volume+volumeOfSmallEnd 

    ! If ‘kerf’ is greater than zero, international 1/8 inch volume as
    ! computed above will be converted to international 1/4 inch volume.
    if (kerf <= 0) then 
        write(*,'(" Volume in Board Feet   = ",f12.2)') volume
        return 
    else
        volume= 0.905*volume 
        write(*,'(" Volume in Board Feet   = ",f12.2)') volume
        return 
    end if
end 

! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! This subroutine was written by professor Wirth, 
! with adjustments made by Jovana Kusic
! to calculate the volume of a saw log in cubic metres
! variables in the calling sequence are:
!       DIBlarge    = dib at log’s large end (inches) (0.0 if 1/2 inch taper)
!       DIBsmall    = log’s scaling diameter (inches)
!       totalLength = total log length (feet)
!       volume      = log volume returned to the calling program
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine calcLOGvolume(DIBlarge, DIBsmall, totalLength, volume)
    implicit none
    real, intent(in) :: DIBsmall, totalLength
    real, intent(inout) :: DIBlarge
    real, intent(out) :: volume
    ! Defines a constant for the value of PI
    real, parameter :: PI = 3.14159265359
    real :: radiusDIBsmall, radiusDIBlarge, totalLengthInMetres, areaOfSmallEnd, areaOfLargeEnd, quotient 

    ! If no DIBlarge value is provided, use the 1/2" per 4 feet metric  
    quotient = totalLength / 4 
    if(DIBlarge <= 0) then
        DIBlarge = DIBsmall + (quotient * 0.5) 
    end if
    ! Determines radius from the diameter given
    radiusDIBsmall = (DIBsmall / 39.37) / 2.0 
    radiusDIBlarge = (DIBlarge / 39.37) / 2.0 
    totalLengthInMetres = totalLength / 3.2808 
    ! Calculates the area of each end of the log using PI*R^2
    areaOfSmallEnd = PI * (radiusDIBsmall*radiusDIBsmall) 
    areaOfLargeEnd = PI * (radiusDIBlarge*radiusDIBlarge) 
    volume = ((areaOfSmallEnd + areaOfLargeEnd) / 2) * totalLengthInMetres 
    write(*,'(" Volume in Cubic Metres = ",f12.2)') volume
    return 
end 


