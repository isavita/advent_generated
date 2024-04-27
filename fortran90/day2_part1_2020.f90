program password_validator
    implicit none
    integer :: i, min, max, count, valid_count
    character(len=1) :: char
    character(len=256) :: policy, password, line, policy_parts(3)
    open(unit=10, file='input.txt', status='old', action='read')

    valid_count = 0
    do
        read(10, '(A)', end=10) line
        i = index(line, ':') - 1
        policy = line(:i)
        password = line(i+2:)

        i = index(policy, '-')
        policy_parts(1) = policy(:i-1)
        policy_parts(2) = policy(i+1:index(policy, ' ')-1)
        policy_parts(3) = policy(index(policy, ' ')+1:)

        read(policy_parts(1), *) min
        read(policy_parts(2), *) max
        char = policy_parts(3)

        count = 0
        do i = 1, len_trim(password)
            if (password(i:i) == char) count = count + 1
        end do
        if (count >= min .and. count <= max) valid_count = valid_count + 1
    end do
10  close(10)
    print *, valid_count
end program password_validator