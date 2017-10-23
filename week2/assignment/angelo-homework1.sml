
(* Should return true if the first param is older,
if they are both the same returns false *)

fun number_is_smaller (n1: int list, n2: int list) =
    if hd n1 = hd n2
    then number_is_smaller (tl n1, tl n2)
    else hd n1 < hd n2

fun is_older (older: int*int*int, newer: int*int*int) =
    if older = newer
    then false
    else number_is_smaller ([#1 older, #2 older, #3 older], [#1 newer, #2 newer, #3 newer])


(* val dates = [[1980,12,16], [1980,04,23], [1980,12,21]];
val months = [1,2,3,4,5,6,7,8,9,10,11,12]; *)

(* takes a list of dates (1st param) and returns how many dates are in that month (2nd param) *)
fun number_in_month (list_of_dates: (int*int*int) list, month: int) =
    if null list_of_dates
    then 0
    else
        if #2 (hd (list_of_dates)) = month
        then 1 + number_in_month (tl list_of_dates, month)
        else 0 + number_in_month (tl list_of_dates, month)


fun number_in_months (list_of_dates: (int*int*int) list, list_of_months: int list) =
    if null list_of_months
    then 0
    else number_in_month (list_of_dates, hd list_of_months)
        +number_in_months (list_of_dates, tl list_of_months)

fun dates_in_month (list_of_dates: (int*int*int) list, month: int) =
    if null list_of_dates
    then []
    else
        if #2 (hd list_of_dates) = month
        then (hd list_of_dates)::dates_in_month(tl list_of_dates, month)
        else dates_in_month (tl list_of_dates, month)

fun dates_in_months (list_of_dates: (int*int*int) list, list_of_months: int list) =
    if null list_of_months
    then []
    else dates_in_month (list_of_dates, hd list_of_months)
        @(dates_in_months (list_of_dates, tl list_of_months))

fun get_nth_helper (msg: string list, offset: int, counter: int) =
    if offset = counter
    then hd msg
    else get_nth_helper(tl msg, offset, counter+1)

fun get_nth (msg: string list, offset: int) =
    get_nth_helper (msg, offset, 1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string (date: int*int*int) =
    get_nth (months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun sum_numbers (sum: int, numbers_list: int list, acc: int, counter: int) =
    if sum <= (acc + (hd numbers_list) + (hd (tl numbers_list)))
    then counter
    else sum_numbers (sum, tl numbers_list, acc+(hd numbers_list), counter + 1)

fun number_before_reaching_sum (sum: int, numbers_list: int list) =
    if null (tl numbers_list)
    then 0
    else sum_numbers (sum, numbers_list, 0, 1)

val number_of_days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun find_month (day_number: int, numbers_list: int list, acc: int, counter: int) =
    if day_number <= (acc + hd numbers_list)
        then counter
        else find_month (day_number, tl numbers_list, acc + (hd numbers_list), counter+1)

fun what_month (day_number: int) =
    if day_number = 0
    then 0
    else find_month (day_number, number_of_days_in_months, 0, 1)

fun print_months_between (day1: int, day2: int, days_months: int list , months_between: int list) =
    if day1 = day2
        then months_between
        else print_months_between (day1+1, day2, days_months, months_between@[ find_month(day1, days_months, 0, 1) ])

fun month_range(day1: int, day2: int) =
    if day1 > day2 orelse day1 = day2
    then []
    else
        let
            val month1 = find_month (day1, number_of_days_in_months, 0, 1)
            val month2 = find_month (day2, number_of_days_in_months, 0, 1)
            val months_between = print_months_between (day1+1, day2, number_of_days_in_months, [])
        in
            [month1]@months_between@[month2]
        end

fun oldest_date (dates: (int*int*int) list, oldest: (int*int*int)) =
    if null dates
    then oldest
    else 
        if is_older(hd dates, oldest)
        then oldest_date (tl dates, hd dates)
        else oldest_date (tl dates, oldest)

fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else SOME (oldest_date (tl dates, hd dates))
