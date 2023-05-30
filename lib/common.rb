def dbg(x, no_inspect: false)
  return unless DEBUG

  if block_given?
    puts yield(x.inspect)
  elsif no_inspect
    puts x
  else
    p x
  end
end

def dbgf(fstr, *args)
  message = sprintf(fstr, *args)
  dbg(message, no_inspect: true)
end

def mysleep(x)
  f = Fiber.current
  EM::Timer.new(x) { f.resume }
  Fiber.yield
end

def wait_until(
  timeout: 5,
  message: nil,
  interval: 0.1,
  debug: false,
  timeout_expectation: nil
)
  while !(result = yield) && timeout > 0
    dbgf("%s (%.1f)\n", message, timeout) if debug
    start = Time.now
    mysleep(interval)
    diff = Time.now - start
    timeout = timeout - diff
  end
  if timeout_expectation
    timeout_expectation.call
  else
    raise "Timed out on: #{message}" unless result
  end
end
