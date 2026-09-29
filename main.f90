program engine
use, intrinsic :: iso_fortran_env
implicit none (type, external)
integer, parameter             :: OFF_BOARD = -1, EMPTY = 0
integer, parameter             :: PAWN_W = 10, PAWN_B = 11
integer(int32), dimension(120) :: board
character(len=128) :: UCI_IN
integer :: idx_rank, idx_file
board = OFF_BOARD
init_board: do idx_file = 1,8
    do idx_rank = 2,9
        board(idx_file + idx_rank*10) = EMPTY
    end do
end do init_board

setup_pawns: do idx_file = 1,8
    board(idx_file+30) = PAWN_W
    board(idx_file+80) = PAWN_B
end do setup_pawns
do 
    read(*,'(A)') UCI_IN
    select case(trim(UCI_IN))
    case('quit')
        exit
    case('uci')
        write(*,'(A)') 'uciok'
    case('d')
        call print_ascii_board()
    case('eval')
        write(*,'(F10.4)') eval_board(board)
    case default
        call do_move(trim(UCI_IN))
    end select
end do
contains

subroutine print_ascii_board()
do idx_rank = 9,2, -1
    do idx_file = 1,8
        write(*,'(I3)', advance='no') board(idx_file + idx_rank*10)
    end do
    write(*,'(A)')''
end do
end subroutine print_ascii_board

real function eval_board(inBoard)
    integer(int32), dimension(120),intent(in) :: inBoard
    real, dimension(120) :: VAL_WPAWN, VAL_BPAWN
    eval_board = 0

    setup_wpawns_val: do idx_file = 1,8
    do idx_rank = 2,9
        VAL_WPAWN(idx_file + idx_rank*10) = (idx_rank-1)*0.6
        VAL_WPAWN(idx_file + 90) = 2000
    end do
    end do setup_wpawns_val

    setup_bpawns_val: do idx_file = 1,8
    do idx_rank = 2,9
        VAL_BPAWN(idx_file + idx_rank*10) = (9-(idx_rank-1))*0.6
        VAL_BPAWN(idx_file + 20) = 2000
    end do
    end do setup_bpawns_val

    eval_wpawn: do idx_file = 1,8
    do idx_rank = 2,9
        if (inBoard(idx_file + idx_rank*10) == PAWN_W) then 
            eval_board = eval_board + VAL_WPAWN(idx_file + idx_rank*10)
        end if
    end do
    end do eval_wpawn

    eval_bpawn: do idx_file = 1,8
    do idx_rank = 2,9
        if (inBoard(idx_file + idx_rank*10) == PAWN_B) then 
            eval_board = eval_board - VAL_BPAWN(idx_file + idx_rank*10)
        end if
    end do
    end do eval_bpawn
end function eval_board

subroutine do_move(coords)
    character(len=*), intent(in) :: coords
    integer :: trans_rank, trans_file
    integer :: trans_rank2, trans_file2
    trans_file = (ICHAR(coords(1:1))-96)
    trans_file2 = (ICHAR(coords(3:3))-96)
    read(coords(2:2),'(I1)') trans_rank
    read(coords(4:4),'(I1)') trans_rank2
    trans_rank = trans_rank + 1
    trans_rank2 = trans_rank2 + 1
    board(trans_file2 + trans_rank2*10) = board(trans_file + trans_rank*10)
    board(trans_file + trans_rank*10) = EMPTY
end subroutine do_move

end program engine
