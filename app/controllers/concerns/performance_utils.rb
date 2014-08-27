# -*- encoding : utf-8 -*-
module PerformanceUtils
  def performance_helper(test_name, &block)
    if ENV['PERFORMANCE_TEST']
      puts "================ PERFORMANCE TEST for #{test_name} ==================="
      Benchmark.bm { |bm|
        bm.report { block.call }
      }
    else
      block.call
    end

  end
end
