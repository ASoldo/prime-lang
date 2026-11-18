idf_component_register(SRCS "app.c" "socket_adapter.c" INCLUDE_DIRS "." PRIV_REQUIRES esp_netif lwip)

if(NOT CMAKE_BUILD_EARLY_EXPANSION)
    if(NOT IDF_TARGET STREQUAL "esp32")
        message(FATAL_ERROR "The Prime socket component requires classic ESP32")
    endif()
    find_program(PRIME_CLI prime-lang REQUIRED)
    find_program(PRIME_ESP_LLC llc REQUIRED)
    get_filename_component(llvm_bin "${PRIME_ESP_LLC}" DIRECTORY)
    find_program(PRIME_ESP_CLANG clang PATHS "${llvm_bin}" NO_DEFAULT_PATH REQUIRED)
    set(app_dir "${CMAKE_CURRENT_SOURCE_DIR}/..")
    set(prime_object "${CMAKE_CURRENT_BINARY_DIR}/idf_net.o")
    set(integer_object "${CMAKE_CURRENT_BINARY_DIR}/sdk_integer.o")
    add_custom_command(OUTPUT "${prime_object}"
        COMMAND "${CMAKE_COMMAND}" -E env "PRIME_LLC=${PRIME_ESP_LLC}"
            "${PRIME_CLI}" build "${app_dir}/main.prime" --emit-object "${prime_object}" --no-flash
        DEPENDS "${PRIME_CLI}" "${app_dir}/main.prime" "${app_dir}/prime.toml"
        VERBATIM)
    # Prime's numeric ABI passes i128. Espressif Clang accepts this extended type on Xtensa.
    add_custom_command(OUTPUT "${integer_object}"
        COMMAND "${PRIME_ESP_CLANG}" --target=xtensa-esp-elf -mcpu=esp32 -fforce-enable-int128
            -Os -c "${CMAKE_CURRENT_SOURCE_DIR}/sdk_integer.c" -o "${integer_object}"
        DEPENDS "sdk_integer.c"
        VERBATIM)
    target_sources(${COMPONENT_LIB} PRIVATE "${prime_object}" "${integer_object}")
endif()
