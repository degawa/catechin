program demo_logging
    use :: catechin
    implicit none

    call logging(Lv%INFO, "test", purpose=Purpose_Trace, category="IO.vector3d.vtr")

    call configure(Purpose_Trace, level=Lv%DEBUG)
    call logging(Lv%DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv%INFO, "info", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv%WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv%ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.debug")

    call configure(Purpose_Trace, level=Lv%INFO)
    call logging(Lv%DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv%INFO, "info", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv%WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv%ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.info")

    call configure(Purpose_Trace, level=Lv%WARN)
    call logging(Lv%DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv%INFO, "info", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv%WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv%ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.warn")

    call configure(Purpose_Trace, level=Lv%ERROR)
    call logging(Lv%DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv%INFO, "info", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv%WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv%ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.error")
end program demo_logging
