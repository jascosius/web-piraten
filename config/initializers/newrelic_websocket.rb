module NewRelic
  module Agent
    module Instrumentation
      module WebsocketRails
        module ActionController
          def self.newrelic_write_attr(attr_name, value) # :nodoc:
            write_inheritable_attribute(attr_name, value)
          end

          def self.newrelic_read_attr(attr_name) # :nodoc:
            read_inheritable_attribute(attr_name)
          end

          # determine the path that is used in the metric name for
          # the called controller action
          def newrelic_metric_path
            "#{self.class.controller_name}/#{action_name}"
          end

          def process_action(*args)
            # skip instrumentation if we are in an ignored action
            if _is_filtered?('do_not_trace')
              NewRelic::Agent.disable_all_tracing do
                return super
              end
            end

            perform_action_with_newrelic_trace(:category => :controller, :name => self.action_name, :path => newrelic_metric_path, :params => {:params => self.data}, :class_name => self.class.name)  do
              super
            end
          end
        end
      end
    end
  end
end

DependencyDetection.defer do
  @name = :websocket_rails_controller

  depends_on do
    defined?(::WebsocketRails)
  end

  executes do
    ::NewRelic::Agent.logger.info 'Installing WebsocketRails Controller instrumentation'
  end

  executes do
    class WebsocketRails::BaseController
      include NewRelic::Agent::Instrumentation::ControllerInstrumentation
      include NewRelic::Agent::Instrumentation::WebsocketRails::ActionController
    end

    class WebsocketRails::InternalController
      newrelic_ignore
    end
  end
end