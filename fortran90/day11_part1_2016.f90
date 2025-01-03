
program rtg_hell_day
  implicit none

  type :: half
    logical :: is_chip
    character(len=20) :: material
  end type half

  type :: state
    type(half), dimension(:,:), allocatable :: floors
    integer :: elevator_level
    integer :: steps
  end type state

  type(state) :: current_state
  integer :: ans

  current_state = new_initial_state("input.txt")
  ans = rtg_hell_day_solver(current_state)
  print *, ans

contains

  function rtg_hell_day_solver(initial_state) result(ans)
    type(state), intent(in) :: initial_state
    integer :: ans
    type(state), dimension(:), allocatable :: queue
    logical, dimension(:), allocatable :: prev_states
    integer :: queue_size, i, j, k, next_states_size
    character(len=1000) :: hash
    type(state), dimension(:), allocatable :: next_states

    queue_size = 1
    allocate(queue(queue_size))
    queue(1) = initial_state

    allocate(prev_states(1000000))
    prev_states = .false.

    do while (queue_size > 0)
      current_state = queue(1)
      queue = queue(2:queue_size)
      queue_size = queue_size - 1

      if (is_done(current_state)) then
        ans = current_state%steps
        deallocate(queue)
        deallocate(prev_states)
        return
      end if

      hash = hash_key(current_state)
      i = hash_to_int(hash)
      if (prev_states(i)) then
        cycle
      end if
      prev_states(i) = .true.

      next_states = get_next_states(current_state)
      next_states_size = size(next_states)

      if (next_states_size > 0) then
        queue_size = queue_size + next_states_size
        allocate(queue(queue_size))
        queue(queue_size-next_states_size+1:queue_size) = next_states
        deallocate(next_states)
      end if
    end do

    ans = -1
    deallocate(queue)
    deallocate(prev_states)
  end function rtg_hell_day_solver

  function new_initial_state(filename) result(initial_state)
    character(len=*), intent(in) :: filename
    type(state) :: initial_state
    character(len=1000) :: line
    integer :: i, j, k, line_index, num_lines, pos, start, finish
    character(len=20), dimension(:), allocatable :: parts
    character(len=20) :: word
    integer, dimension(4) :: floor_sizes
    
    open(unit=10, file=filename, status='old', action='read')
    num_lines = 0
    do
      read(10, '(A)', iostat=i) line
      if (i /= 0) exit
      num_lines = num_lines + 1
    end do
    close(10)

    allocate(initial_state%floors(num_lines, 0))
    initial_state%elevator_level = 0
    initial_state%steps = 0
    floor_sizes = 0

    open(unit=10, file=filename, status='old', action='read')
    do line_index = 1, num_lines
      read(10, '(A)') line
      
      i = 1
      j = 1
      do
        pos = index(line(i:), ' ')
        if (pos == 0) then
          allocate(parts(j))
          parts(j) = trim(line(i:))
          exit
        end if
        allocate(parts(j))
        parts(j) = trim(line(i:i+pos-2))
        i = i + pos
        j = j + 1
      end do
      
      do i = 1, size(parts)
        word = parts(i)
        if (word == "generator") then
          floor_sizes(line_index) = floor_sizes(line_index) + 1
          allocate(initial_state%floors(line_index, floor_sizes(line_index)))
          initial_state%floors(line_index, floor_sizes(line_index))%is_chip = .false.
          initial_state%floors(line_index, floor_sizes(line_index))%material = parts(i-1)
        else if (word == "microchip") then
          start = 1
          finish = index(parts(i-1), "-comp") - 1
          floor_sizes(line_index) = floor_sizes(line_index) + 1
          allocate(initial_state%floors(line_index, floor_sizes(line_index)))
          initial_state%floors(line_index, floor_sizes(line_index))%is_chip = .true.
          initial_state%floors(line_index, floor_sizes(line_index))%material = parts(i-1)(start:finish)
        end if
      end do
      deallocate(parts)
    end do
    close(10)
  end function new_initial_state

  function hash_key(s) result(hash)
    type(state), intent(in) :: s
    character(len=1000) :: hash
    integer :: i, j, k, gen_index, chip_index, num_pairs
    integer, dimension(:,:), allocatable :: gen_chip_pairs
    character(len=20), dimension(:), allocatable :: materials
    integer, dimension(:), allocatable :: gen_indices, chip_indices
    integer :: mat_count
    
    mat_count = 0
    do i = 1, size(s%floors, 1)
      do j = 1, size(s%floors, 2)
        if (s%floors(i,j)%is_chip) then
          mat_count = mat_count + 1
        end if
      end do
    end do
    allocate(materials(mat_count))
    allocate(gen_indices(mat_count))
    allocate(chip_indices(mat_count))
    
    mat_count = 0
    do i = 1, size(s%floors, 1)
      do j = 1, size(s%floors, 2)
        if (s%floors(i,j)%is_chip) then
          mat_count = mat_count + 1
          materials(mat_count) = s%floors(i,j)%material
          chip_indices(mat_count) = i
        end if
      end do
    end do
    
    mat_count = 0
    do i = 1, size(s%floors, 1)
      do j = 1, size(s%floors, 2)
        if (.not. s%floors(i,j)%is_chip) then
          do k = 1, size(materials)
            if (s%floors(i,j)%material == materials(k)) then
              gen_indices(k) = i
              exit
            end if
          end do
        end if
      end do
    end do
    
    num_pairs = size(materials)
    allocate(gen_chip_pairs(num_pairs, 2))
    do i = 1, num_pairs
      gen_chip_pairs(i, 1) = gen_indices(i)
      gen_chip_pairs(i, 2) = chip_indices(i)
    end do
    
    call sort_pairs(gen_chip_pairs)
    
    write(hash, '(I0, 2(I0,I0))') s%elevator_level, ((gen_chip_pairs(i,1), gen_chip_pairs(i,2)), i=1,num_pairs)
    deallocate(materials)
    deallocate(gen_indices)
    deallocate(chip_indices)
    deallocate(gen_chip_pairs)
  end function hash_key

  subroutine sort_pairs(pairs)
    integer, dimension(:,:), intent(inout) :: pairs
    integer :: i, j, temp1, temp2
    do i = 1, size(pairs, 1) - 1
      do j = i + 1, size(pairs, 1)
        if (pairs(j, 1) < pairs(i, 1) .or. (pairs(j, 1) == pairs(i, 1) .and. pairs(j, 2) < pairs(i, 2))) then
          temp1 = pairs(i, 1)
          temp2 = pairs(i, 2)
          pairs(i, 1) = pairs(j, 1)
          pairs(i, 2) = pairs(j, 2)
          pairs(j, 1) = temp1
          pairs(j, 2) = temp2
        end if
      end do
    end do
  end subroutine sort_pairs

  function hash_to_int(hash) result(int_hash)
    character(len=*), intent(in) :: hash
    integer :: int_hash, i, len
    int_hash = 0
    len = len_trim(hash)
    do i = 1, len
      int_hash = int_hash * 31 + ichar(hash(i:i))
    end do
    int_hash = mod(int_hash, 1000000) + 1
  end function hash_to_int

  function is_valid(s) result(valid)
    type(state), intent(in) :: s
    logical :: valid
    integer :: i, j, k, num_gens
    character(len=20), dimension(:), allocatable :: gens_seen
    
    valid = .true.
    do i = 1, size(s%floors, 1)
      num_gens = 0
      do j = 1, size(s%floors, 2)
        if (.not. s%floors(i,j)%is_chip) then
          num_gens = num_gens + 1
        end if
      end do
      
      if (num_gens == 0) cycle
      
      allocate(gens_seen(num_gens))
      num_gens = 0
      do j = 1, size(s%floors, 2)
        if (.not. s%floors(i,j)%is_chip) then
          num_gens = num_gens + 1
          gens_seen(num_gens) = s%floors(i,j)%material
        end if
      end do
      
      do j = 1, size(s%floors, 2)
        if (s%floors(i,j)%is_chip) then
          do k = 1, size(gens_seen)
            if (s%floors(i,j)%material == gens_seen(k)) exit
          end do
          if (k > size(gens_seen)) then
            valid = .false.
            deallocate(gens_seen)
            return
          end if
        end if
      end do
      deallocate(gens_seen)
    end do
  end function is_valid

  function is_done(s) result(done)
    type(state), intent(in) :: s
    logical :: done
    integer :: i, len_sum
    
    len_sum = 0
    do i = 1, 3
      len_sum = len_sum + size(s%floors, 2, dim=i)
    end do
    done = (len_sum == 0)
  end function is_done

  function get_movable_perm_indices(s) result(perms)
    type(state), intent(in) :: s
    integer, dimension(:,:), allocatable :: perms
    integer :: i, j, k, current_level_size, num_perms
    
    current_level_size = size(s%floors, 2, dim=s%elevator_level+1)
    num_perms = current_level_size * (current_level_size - 1) / 2 + current_level_size
    allocate(perms(num_perms, 2))
    k = 0
    do i = 1, current_level_size
      do j = i + 1, current_level_size
        k = k + 1
        perms(k, 1) = i
        perms(k, 2) = j
      end do
    end do
    do i = 1, current_level_size
      k = k + 1
      perms(k, 1) = i
      perms(k, 2) = 0
    end do
    perms = perms(1:k,:)
  end function get_movable_perm_indices

  function clone_state(s) result(cloned_state)
    type(state), intent(in) :: s
    type(state) :: cloned_state
    integer :: i, j
    
    cloned_state%elevator_level = s%elevator_level
    cloned_state%steps = s%steps
    allocate(cloned_state%floors(size(s%floors, 1), 0))
    do i = 1, size(s%floors, 1)
      do j = 1, size(s%floors, 2, dim=i)
        allocate(cloned_state%floors(i, j))
        cloned_state%floors(i, j) = s%floors(i, j)
      end do
    end do
  end function clone_state

  function get_next_states(s) result(future_states)
    type(state), intent(in) :: s
    type(state), dimension(:), allocatable :: future_states
    integer, dimension(:,:), allocatable :: movable_perm_indices
    integer :: i, j, k, ele_diff, num_perms, next_states_size
    integer, dimension(:), allocatable :: ele_diffs
    type(state) :: cl
    
    movable_perm_indices = get_movable_perm_indices(s)
    num_perms = size(movable_perm_indices, 1)
    
    if (s%elevator_level < size(s%floors, 1) - 1) then
      allocate(ele_diffs(2))
      ele_diffs(1) = 1
      ele_diffs(2) = -1
    else if (s%elevator_level > 0) then
      allocate(ele_diffs(1))
      ele_diffs(1) = -1
    else
      allocate(ele_diffs(0))
    end if
    
    next_states_size = 0
    do i = 1, size(ele_diffs)
      ele_diff = ele_diffs(i)
      do j = 1, num_perms
        cl = clone_state(s)
        cl%elevator_level = cl%elevator_level + ele_diff
        cl%steps = cl%steps + 1
        
        do k = 1, 2
          if (movable_perm_indices(j, k) > 0) then
            allocate(cl%floors(cl%elevator_level+1, size(cl%floors, 2, dim=cl%elevator_level+1)+1))
            cl%floors(cl%elevator_level+1, size(cl%floors, 2, dim=cl%elevator_level+1)) = s%floors(s%elevator_level+1, movable_perm_indices(j, k))
          end if
        end do
        
        do k = 2, 1, -1
          if (movable_perm_indices(j, k) > 0) then
            s%floors(s%elevator_level+1, movable_perm_indices(j, k)) = s%floors(s%elevator_level+1, size(s%floors, 2, dim=s%elevator_level+1))
            deallocate(s%floors(s%elevator_level+1, size(s%floors, 2, dim=s%elevator_level+1)))
          end if
        end do
        
        if (is_valid(cl)) then
          next_states_size = next_states_size + 1
          if (next_states_size == 1) then
            allocate(future_states(next_states_size))
          else
            allocate(future_states(next_states_size))
          end if
          future_states(next_states_size) = cl
        end if
      end do
    end do
    
    if (allocated(ele_diffs)) deallocate(ele_diffs)
    deallocate(movable_perm_indices)
  end function get_next_states
end program rtg_hell_day
