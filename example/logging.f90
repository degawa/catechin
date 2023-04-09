program demo_logging
    use :: catechin
    implicit none

    call logging(Lv_INFO, "test", purpose=Purpose_Trace, category="IO.vector3d.vtr")

    call configure(Purpose_Trace, level=Lv_DEBUG)
    call logging(Lv_DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv_INFO, "info", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv_WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.debug")
    call logging(Lv_ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.debug")

    call configure(Purpose_Trace, level=Lv_INFO)
    call logging(Lv_DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv_INFO, "info", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv_WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.info")
    call logging(Lv_ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.info")

    call configure(Purpose_Trace, level=Lv_WARN)
    call logging(Lv_DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv_INFO, "info", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv_WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.warn")
    call logging(Lv_ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.warn")

    call configure(Purpose_Trace, level=Lv_ERROR)
    call logging(Lv_DEBUG, "debug", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv_INFO, "info", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv_WARN, "warn", purpose=Purpose_Trace, category="test.configure.level.error")
    call logging(Lv_ERROR, "error", purpose=Purpose_Trace, category="test.configure.level.error")
end program demo_logging
