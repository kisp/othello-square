module ServiceManager
  class Service
    alias_method :stop_orig, :stop

    def stop
      Kernel.eval "
class ::File
  alias_method :eof_orig?, :eof?

  def eof?
    eof_orig?
   rescue
     true
  end
end
      "
      stop_orig
    end
  end
end
