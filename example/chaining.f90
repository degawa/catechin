program demo_chaining
    use :: catechin
    implicit none

    call logging(logger(Purpose_Report, Lv_INFO, "category"))

    call logging(logger(Purpose_Report, Lv_INFO, "category") &
                 .message."log message")

    call logging(logger(Purpose_Report, Lv_INFO, "category") .in.module("module") &
                 .message."log message")

    call logging(logger(Purpose_Report, Lv_INFO, "category") .in.procedure("procedure") &
                 .message."log message")

    call logging(logger(Purpose_Report, Lv_INFO, "category") .in.caller("module", "procedure") &
                 .message."log message")
end program demo_chaining
