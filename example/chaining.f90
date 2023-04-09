program demo_chaining
    use :: catechin
    implicit none

    call logging(logger(Pur%Report, Lv%INFO, "category"))

    call logging(logger(Pur%Report, Lv%INFO, "category") &
                 .message."log message")

    call logging(logger(Pur%Report, Lv%INFO, "category") .in.module("module") &
                 .message."log message")

    call logging(logger(Pur%Report, Lv%INFO, "category") .in.procedure("procedure") &
                 .message."log message")

    call logging(logger(Pur%Report, Lv%INFO, "category") .in.caller("module", "procedure") &
                 .message."log message")
end program demo_chaining
