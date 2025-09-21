cmake_minimum_required(VERSION 3.20)

project(unicorn C)

set(CMAKE_C_STANDARD 23)

set(UNICORN_VERSION_MAJOR 1)
set(UNICORN_VERSION_MINOR 0)
set(UNICORN_VERSION_PATCH 0)

option(UNICORN_LOGGING "Enable logging" OFF)
set(UNICORN_ARCH "riscv" CACHE STRING "Enabled unicorn architectures")
option(UNICORN_TRACER "Trace unicorn execution" OFF)

set(UNICORN_HAS_RISCV TRUE)

include_directories(
    ${CMAKE_BINARY_DIR}
)

include_directories(
    glib_compat
    qemu
    qemu/include
    include
    qemu/tcg
)

# QEMU logging facility
if (UNICORN_LOGGING)
    add_compile_options(-DUNICORN_LOGGING)
endif()

set(UNICORN_TARGET_ARCH "riscv")
set(ATOMIC_LINKAGE_FIX TRUE)
set(EXTRA_CFLAGS "${EXTRA_CFLAGS}-DUNICORN_HAS_RISCV ")
set(TARGET_LIST "${TARGET_LIST}riscv32-softmmu, ")
execute_process(COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/qemu/scripts/create_config
    INPUT_FILE ${CMAKE_BINARY_DIR}/riscv32-softmmu/config-target.mak
    OUTPUT_FILE ${CMAKE_BINARY_DIR}/riscv32-softmmu/config-target.h
)
execute_process(COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/qemu/scripts/create_config
    INPUT_FILE ${CMAKE_BINARY_DIR}/riscv64-softmmu/config-target.mak
    OUTPUT_FILE ${CMAKE_BINARY_DIR}/riscv64-softmmu/config-target.h
)
add_compile_options(
    ${UNICORN_CFLAGS}
    -I${CMAKE_CURRENT_SOURCE_DIR}/qemu/tcg/${UNICORN_TARGET_ARCH}
    -D_GNU_SOURCE
    -D_FILE_OFFSET_BITS=64
    -D_LARGEFILE_SOURCE
    -Wall
    -fPIC
)

set(UNICORN_ARCH_COMMON
    qemu/exec.c
    qemu/exec-vary.c

    qemu/softmmu/cpus.c
    qemu/softmmu/ioport.c
    qemu/softmmu/memory.c
    qemu/softmmu/memory_mapping.c

    qemu/fpu/softfloat.c

    qemu/tcg/optimize.c
    qemu/tcg/tcg.c
    qemu/tcg/tcg-op.c
    qemu/tcg/tcg-op-gvec.c
    qemu/tcg/tcg-op-vec.c

    qemu/accel/tcg/cpu-exec.c
    qemu/accel/tcg/cpu-exec-common.c
    qemu/accel/tcg/cputlb.c
    qemu/accel/tcg/tcg-all.c
    qemu/accel/tcg/tcg-runtime.c
    qemu/accel/tcg/tcg-runtime-gvec.c
    qemu/accel/tcg/translate-all.c
    qemu/accel/tcg/translator.c

    qemu/softmmu/unicorn_vtlb.c
)




add_library(riscv32-softmmu STATIC
    ${UNICORN_ARCH_COMMON}

    qemu/target/riscv/cpu.c
    qemu/target/riscv/cpu_helper.c
    qemu/target/riscv/csr.c
    qemu/target/riscv/fpu_helper.c
    qemu/target/riscv/op_helper.c
    qemu/target/riscv/pmp.c
    qemu/target/riscv/translate.c
    qemu/target/riscv/unicorn.c
)

target_compile_options(riscv32-softmmu PRIVATE
    -DNEED_CPU_H
    -include riscv32.h
    -I${CMAKE_BINARY_DIR}/riscv32-softmmu
    -I${CMAKE_CURRENT_SOURCE_DIR}/qemu/target/riscv
)

target_compile_options(riscv32-softmmu PRIVATE -DUNICORN_TRACER)




set(UNICORN_SRCS
    uc.c
    qemu/softmmu/vl.c
    qemu/hw/core/cpu.c
)

set(UNICORN_COMMON_SRCS

    list.c

    glib_compat/glib_compat.c
    glib_compat/gtestutils.c
    glib_compat/garray.c
    glib_compat/gtree.c
    glib_compat/grand.c
    glib_compat/glist.c
    glib_compat/gmem.c
    glib_compat/gpattern.c
    glib_compat/gslice.c

    qemu/util/bitmap.c
    qemu/util/bitops.c
    qemu/util/crc32c.c
    qemu/util/cutils.c
    qemu/util/getauxval.c
    qemu/util/guest-random.c
    qemu/util/host-utils.c
    qemu/util/osdep.c
    qemu/util/qdist.c
    qemu/util/qemu-timer.c
    qemu/util/qemu-timer-common.c
    qemu/util/range.c
    qemu/util/qht.c
    qemu/util/pagesize.c
    qemu/util/cacheinfo.c

    qemu/crypto/aes.c
)

set(UNICORN_COMMON_SRCS
    ${UNICORN_COMMON_SRCS}
    qemu/util/oslib-posix.c
    qemu/util/qemu-thread-posix.c
)

add_library(unicorn-common STATIC
    ${UNICORN_COMMON_SRCS}
)

set_target_properties(unicorn-common
                      PROPERTIES
                      C_VISIBILITY_PRESET
                      hidden)

target_link_libraries(unicorn-common PRIVATE pthread)

add_library(unicorn STATIC ${UNICORN_SRCS})

set(UNICORN_LINK_LIBRARIES ${UNICORN_LINK_LIBRARIES} unicorn-common)
set(UNICORN_COMPILE_OPTIONS ${UNICORN_COMPILE_OPTIONS} -DUNICORN_HAS_RISCV)
set(UNICORN_LINK_LIBRARIES ${UNICORN_LINK_LIBRARIES} riscv32-softmmu riscv64-softmmu)
set(UNICORN_SAMPLE_FILE ${UNICORN_SAMPLE_FILE} sample_riscv)
target_link_libraries(riscv32-softmmu PRIVATE unicorn-common)
set(UNICORN_TEST_FILE ${UNICORN_TEST_FILE} test_riscv)
