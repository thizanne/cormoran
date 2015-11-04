module type Context = sig
  type 'a t
end

module Identity : Context = struct
  type 'a t = 'a
end

module Threaded : Context = struct
  type 'a t = {
    item : 'a;
    thread_id : Program.thread_id;
  }
end
