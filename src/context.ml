module type Context = sig
  type 'a t
end

module Identity = struct
  type 'a t = 'a
end

module Threaded = struct
  type 'a t = {
    item : 'a;
    thread_id : Program.thread_id;
  }
end

module MaybeThreaded = struct
  type 'a t = {
    item : 'a;
    thread_id : Program.thread_id option;
  }
end
