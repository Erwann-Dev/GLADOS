import React from 'react';

const SystemCalls: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">System Calls</h1>

        {/* System Call Interface Section */}
        <h2 className="text-2xl font-semibold mb-2">System Call Interface</h2>
        <p className="text-lg leading-relaxed mb-4">
          Since Ç is primarily targeted at Linux systems, it uses the Linux system call interface. The language's pointers are real machine pointers, making them suitable for direct system calls without any additional overhead.
        </p>

        {/* Example: Write System Call Section */}
        <h2 className="text-2xl font-semibold mb-2">Example: Write System Call</h2>
        <p className="text-lg leading-relaxed mb-4">
          Here's an example of using the write system call to output text to stdout:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// System call numbers for x86_64 Linux
#define SYS_write 1
#define STDOUT 1

void write_example() {
    ptr u8 message = "Hello, World!\\n"
    u64 length = 14  // length of message
    syscall(SYS_write, STDOUT, message, length)

    // the syscall interface is:
    // syscall(%rax, %rdi, %rsi, %rdx, %r10, %r8, %r9)
    // some registers can be omitted.
    // the size of all arguments must be 8 bytes (U64, i64, or pointers)
}`}
          </code>
        </pre>

        {/* Safety Considerations Section */}
        <h2 className="text-2xl font-semibold mb-2">Safety Considerations</h2>
        <p className="text-lg leading-relaxed mb-4">
          When working with system calls, keep the following safety considerations in mind:
        </p>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg">Always check return values for errors</li>
          <li className="text-lg">Ensure buffer sizes are appropriate</li>
          <li className="text-lg">Be careful with file descriptors and memory management</li>
          <li className="text-lg">Consider using higher-level abstractions when possible (once the standard library is available)</li>
        </ul>

        {/* Common System Calls Section */}
        <h2 className="text-2xl font-semibold mb-2">Common System Calls</h2>
        <p className="text-lg leading-relaxed mb-4">
          Here are some commonly used system calls in Ç:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// System call numbers for x86_64 Linux
#define SYS_read    0
#define SYS_write   1
#define SYS_open    2
#define SYS_close   3
#define SYS_stat    4
#define SYS_fstat   5
#define SYS_lstat   6
#define SYS_poll    7
#define SYS_mmap    9
#define SYS_munmap  11
#define SYS_brk     12`}
          </code>
        </pre>
        <p className="text-lg leading-relaxed mb-4">
          These system calls can be used for various operations:
        </p>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg">File I/O operations</li>
          <li className="text-lg">Process management</li>
          <li className="text-lg">Memory management</li>
          <li className="text-lg">Network operations</li>
          <li className="text-lg">Inter-process communication</li>
        </ul>
      </section>
    </div>
  );
};

export default SystemCalls;
