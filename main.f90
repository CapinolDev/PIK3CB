program engine
    use, intrinsic :: iso_fortran_env
    implicit none (type, external)

    integer, parameter             :: SIDE_WHITE = 1, SIDE_BLACK = 2
    integer, parameter             :: INFINITY = 100000, WIN_SCORE = 10000
    integer, parameter             :: OFF_BOARD = -1, EMPTY = 0
    integer, parameter             :: PAWN_W = 10, PAWN_B = 11

    integer(int32), dimension(120) :: board
    integer                        :: currside
    integer                        :: en_passant_sq

    character(len=4096)             :: UCI_IN
    integer                        :: idx_rank, idx_file, read_stat

    currside = SIDE_WHITE
    en_passant_sq = 0

    board = OFF_BOARD
    init_board: do idx_file = 1, 8
        do idx_rank = 2, 9
            board(idx_file + idx_rank * 10) = EMPTY
        end do
    end do init_board

    setup_pawns: do idx_file = 1, 8
        board(idx_file + 30) = PAWN_W  
        board(idx_file + 80) = PAWN_B  
    end do setup_pawns

    uci: do 
        read(*, '(A)', iostat=read_stat) UCI_IN
        if (read_stat /= 0) exit

        if (UCI_IN(1:2) == 'go') then
            call search_root(6, currside)
            cycle
        end if

        if (UCI_IN(1:8) == 'position') then
            call reset_board_to_start()
            call parse_position_moves(trim(UCI_IN))
            cycle
        end if

        select case(trim(UCI_IN))
        case('quit')
            exit
        case('uci')
            write(*, '(A)') 'uciok'
        case('isready')
            write(*, '(A)') 'readyok'
        case('ucinewgame')
            call reset_board_to_start()
        case('d')
            call print_ascii_board()
        case('eval')
            write(*, '(I6)') eval_board(board)
        case default
            if (len_trim(UCI_IN) == 4) then
                call do_move(trim(UCI_IN))
                currside = 3 - currside 
            end if
        end select
    end do uci

contains

    subroutine print_ascii_board()
        implicit none
        integer :: r_idx, f_idx
        do r_idx = 9, 2, -1
            do f_idx = 1, 8
                write(*, '(I3)', advance='no') board(f_idx + r_idx * 10)
            end do
            write(*, '(A)') ''
        end do
    end subroutine print_ascii_board

    integer function eval_board(inBoard)
        implicit none
        integer(int32), dimension(120), intent(in) :: inBoard
        integer :: f_idx, r_idx, sq
        eval_board = 0

        do r_idx = 2, 9
            do f_idx = 1, 8
                sq = f_idx + r_idx * 10
                if (inBoard(sq) == PAWN_W) then
                    eval_board = eval_board + 100 + (r_idx - 2) * 10
                else if (inBoard(sq) == PAWN_B) then
                    eval_board = eval_board - (100 + (9 - r_idx) * 10)
                end if
            end do
        end do
    end function eval_board

    subroutine do_move_move(movefrom, moveto)
        implicit none
        integer, intent(in) :: movefrom, moveto
        integer :: moving_piece

        moving_piece = board(movefrom)

        if (board(moveto) == EMPTY .and. (abs(moveto - movefrom) == 9 .or. abs(moveto - movefrom) == 11)) then
            if (moving_piece == PAWN_W) then
                board(moveto - 10) = EMPTY
            else if (moving_piece == PAWN_B) then
                board(moveto + 10) = EMPTY
            end if
        end if

        board(moveto)   = moving_piece
        board(movefrom) = EMPTY

        if (abs(moveto - movefrom) == 20) then
            en_passant_sq = (movefrom + moveto) / 2
        else
            en_passant_sq = 0
        end if
    end subroutine do_move_move

    subroutine undo_move_move(from_sq, to_sq, captured_piece, old_ep_sq)
        implicit none
        integer, intent(in) :: from_sq, to_sq, captured_piece, old_ep_sq

        board(from_sq) = board(to_sq)
        board(to_sq)   = captured_piece

        if (captured_piece == EMPTY .and. (abs(to_sq - from_sq) == 9 .or. abs(to_sq - from_sq) == 11)) then
            if (board(from_sq) == PAWN_W) then
                board(to_sq - 10) = PAWN_B
            else if (board(from_sq) == PAWN_B) then
                board(to_sq + 10) = PAWN_W
            end if
        end if

        en_passant_sq = old_ep_sq
    end subroutine undo_move_move

    subroutine do_move(coords)
        implicit none
        character(len=*), intent(in) :: coords
        integer :: trans_rank, trans_file
        integer :: trans_rank2, trans_file2
        integer :: movefrom, moveto

        trans_file  = (ICHAR(coords(1:1)) - 96)
        trans_file2 = (ICHAR(coords(3:3)) - 96)
        read(coords(2:2), '(I1)') trans_rank
        read(coords(4:4), '(I1)') trans_rank2

        trans_rank  = trans_rank + 1
        trans_rank2 = trans_rank2 + 1

        movefrom = trans_file  + trans_rank  * 10
        moveto   = trans_file2 + trans_rank2 * 10

        call do_move_move(movefrom, moveto)
    end subroutine do_move

    subroutine generate_pawn_moves(side_to_move, moves_list, move_count)
        implicit none
        integer, intent(in)  :: side_to_move 
        integer, intent(out) :: moves_list(100, 2) 
        integer, intent(out) :: move_count

        integer :: sq, file_idx, rank_idx, target_sq
        integer :: my_pawn, enemy_pawn
        integer :: push_dir, double_push_dir, cap_l, cap_r
        integer :: start_rank_min, start_rank_max

        move_count = 0

        if (side_to_move == SIDE_WHITE) then
            my_pawn         = PAWN_W
            enemy_pawn      = PAWN_B
            push_dir        = 10
            double_push_dir = 20
            cap_l           = 9
            cap_r           = 11
            start_rank_min  = 31 
            start_rank_max  = 38
        else
            my_pawn         = PAWN_B
            enemy_pawn      = PAWN_W
            push_dir        = -10
            double_push_dir = -20
            cap_l           = -11
            cap_r           = -9
            start_rank_min  = 81
            start_rank_max  = 88
        end if

        do rank_idx = 2, 9
            do file_idx = 1, 8
                sq = file_idx + rank_idx * 10

                if (board(sq) == my_pawn) then

                    target_sq = sq + push_dir
                    if (board(target_sq) == EMPTY) then
                        move_count = move_count + 1
                        moves_list(move_count, 1) = sq
                        moves_list(move_count, 2) = target_sq

                        if (sq >= start_rank_min .and. sq <= start_rank_max) then
                            if (board(sq + double_push_dir) == EMPTY) then
                                move_count = move_count + 1
                                moves_list(move_count, 1) = sq
                                moves_list(move_count, 2) = sq + double_push_dir
                            end if
                        end if
                    end if

                    target_sq = sq + cap_l
                    if (board(target_sq) == enemy_pawn .or. target_sq == en_passant_sq) then
                        move_count = move_count + 1
                        moves_list(move_count, 1) = sq
                        moves_list(move_count, 2) = target_sq
                    end if

                    target_sq = sq + cap_r
                    if (board(target_sq) == enemy_pawn .or. target_sq == en_passant_sq) then
                        move_count = move_count + 1
                        moves_list(move_count, 1) = sq
                        moves_list(move_count, 2) = target_sq
                    end if

                end if
            end do
        end do
    end subroutine generate_pawn_moves

    recursive function minimax_search(depth, alpha, beta, side) result(res)
        implicit none
        integer, intent(in)    :: depth, side
        integer, intent(inout) :: alpha, beta
        integer                :: res

        integer :: local_movelist(100, 2)
        integer :: local_movecount
        integer :: opposite_side 
        integer :: move_idx
        integer :: currmovefrom, currmoveto
        integer :: score
        integer :: best_score 
        integer :: captured_piece, old_ep_sq
        integer :: local_alpha, local_beta

        local_alpha = alpha
        local_beta  = beta

        if (check_win_condition() /= 0) then
            if (side == SIDE_WHITE) then
                res = -WIN_SCORE - depth 
            else
                res = WIN_SCORE + depth 
            end if
            return
        end if

        if (depth == 0) then 
            res = eval_board(board)
            return
        end if

        if (side == SIDE_WHITE) then 
            opposite_side = SIDE_BLACK
            best_score    = -INFINITY
        else 
            opposite_side = SIDE_WHITE
            best_score    = INFINITY
        end if
        
        call generate_pawn_moves(side, local_movelist, local_movecount)

        if (local_movecount == 0) then
            res = eval_board(board)
            return
        end if

        do move_idx = 1, local_movecount
            currmovefrom = local_movelist(move_idx, 1)
            currmoveto   = local_movelist(move_idx, 2)

            captured_piece = board(currmoveto)
            old_ep_sq      = en_passant_sq
            
            call do_move_move(currmovefrom, currmoveto)

            score = minimax_search(depth - 1, local_alpha, local_beta, opposite_side)

            call undo_move_move(currmovefrom, currmoveto, captured_piece, old_ep_sq)

            if (side == SIDE_WHITE) then 
                best_score  = max(best_score, score)
                local_alpha = max(local_alpha, best_score)
            else 
                best_score  = min(best_score, score)
                local_beta  = min(local_beta, best_score)
            end if    

            if (local_alpha >= local_beta) exit

        end do

        res = best_score
    end function minimax_search

    subroutine search_root(search_depth, side)
        implicit none
        integer, intent(in) :: search_depth, side
        integer          :: root_movelist(100, 2)
        integer          :: root_movecount, move_idx
        integer          :: currmovefrom, currmoveto
        integer          :: best_from, best_to
        integer          :: score, best_score
        integer          :: alpha, beta
        integer          :: captured_piece, old_ep_sq
        integer          :: opposite_side
        character(len=4) :: best_uci_str

        alpha = -INFINITY
        beta  = INFINITY

        if (side == SIDE_WHITE) then
            opposite_side = SIDE_BLACK
            best_score    = -INFINITY
        else
            opposite_side = SIDE_WHITE
            best_score    = INFINITY
        end if

        call generate_pawn_moves(side, root_movelist, root_movecount)

        if (root_movecount == 0) then
            write(*, '(A)') 'bestmove (none)'
            return
        end if

        best_from = root_movelist(1, 1)
        best_to   = root_movelist(1, 2)

        do move_idx = 1, root_movecount
            currmovefrom = root_movelist(move_idx, 1)
            currmoveto   = root_movelist(move_idx, 2)

            captured_piece = board(currmoveto)
            old_ep_sq      = en_passant_sq

            call do_move_move(currmovefrom, currmoveto)

            score = minimax_search(search_depth - 1, alpha, beta, opposite_side)

            call undo_move_move(currmovefrom, currmoveto, captured_piece, old_ep_sq)

            if (side == SIDE_WHITE) then
                if (score > best_score) then
                    best_score = score
                    best_from  = currmovefrom
                    best_to    = currmoveto
                end if
                alpha = max(alpha, best_score)
            else
                if (score < best_score) then
                    best_score = score
                    best_from  = currmovefrom
                    best_to    = currmoveto
                end if
                beta = min(beta, best_score)
            end if
        end do

        best_uci_str = move_to_uci(best_from, best_to)
        write(*, '(A, A)') 'bestmove ', best_uci_str

        call do_move_move(best_from, best_to)
        if (currside == SIDE_WHITE) then
            currside = SIDE_BLACK
        else
            currside = SIDE_WHITE
        end if
    end subroutine search_root

    function move_to_uci(from_sq, to_sq) result(str)
        implicit none
        integer, intent(in) :: from_sq, to_sq
        character(len=4)    :: str
        integer             :: f1, r1, f2, r2

        f1 = mod(from_sq, 10)
        r1 = (from_sq / 10) - 1
        f2 = mod(to_sq, 10)
        r2 = (to_sq / 10) - 1

        str(1:1) = ACHAR(96 + f1)
        str(2:2) = ACHAR(48 + r1)
        str(3:3) = ACHAR(96 + f2)
        str(4:4) = ACHAR(48 + r2)
    end function move_to_uci

    function check_win_condition() result(winner)
        implicit none
        integer :: winner 
        integer :: f, white_pawns, black_pawns
        integer :: sq_rank8, sq_rank1

        winner = 0
        white_pawns = 0
        black_pawns = 0

        do f = 1, 8
            sq_rank8 = f + 90
            sq_rank1 = f + 20 

            if (board(sq_rank8) == PAWN_W) then
                winner = SIDE_WHITE
                return
            end if

            if (board(sq_rank1) == PAWN_B) then
                winner = SIDE_BLACK
                return
            end if
        end do

        do f = 21, 98
            if (board(f) == PAWN_W) white_pawns = white_pawns + 1
            if (board(f) == PAWN_B) black_pawns = black_pawns + 1
        end do

        if (black_pawns == 0 .and. white_pawns > 0) then
            winner = SIDE_WHITE
        else if (white_pawns == 0 .and. black_pawns > 0) then
            winner = SIDE_BLACK
        end if
    end function check_win_condition

    subroutine reset_board_to_start()
        implicit none
        integer :: f, r
        currside = SIDE_WHITE
        en_passant_sq = 0
        board = OFF_BOARD
        
        do f = 1, 8
            do r = 2, 9
                board(f + r * 10) = EMPTY
            end do
            board(f + 30) = PAWN_W  
            board(f + 80) = PAWN_B  
        end do
    end subroutine reset_board_to_start

    subroutine parse_position_moves(cmd)
        implicit none
        character(len=*), intent(in) :: cmd
        integer :: moves_pos, i
        character(len=4096) :: moves_str
        character(len=4)   :: move_token

        moves_pos = index(cmd, 'moves ')
        if (moves_pos == 0) return 

        moves_str = adjustl(cmd(moves_pos + 6:))
        
        do while (len_trim(moves_str) >= 4)
            move_token = moves_str(1:4)
            call do_move(move_token)
            currside = 3 - currside
            
            if (len_trim(moves_str) > 5) then
                moves_str = adjustl(moves_str(6:))
            else
                exit
            end if
        end do
    end subroutine parse_position_moves

end program engine