
use std::{thread, sync::{mpsc, Arc, Mutex}};

use lsp_server::RequestId;

use crate::utils::ILoggerV2;


struct Job{
    pub run: Box<dyn FnOnce() + Send + 'static>,
    pub req_id: Option<RequestId>
}
impl Job {
    pub fn new(func:Box<dyn FnOnce() + Send + 'static>, req_id: Option<RequestId>) -> Job{
        Job{
            run: func,
            req_id
        }
    }
}

enum Message {
    NewJob(Job),
    Terminate,
}
#[derive(Debug)]
pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Message>,
    logger: Box<dyn ILoggerV2>
}

impl ThreadPool {
    // --snip--
    pub fn new(size: usize, logger: Box<dyn ILoggerV2>) -> ThreadPool {
        assert!(size > 0);

        let (sender, receiver) = mpsc::channel::<Message>();

        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);

        for id in 0..size {
            workers.push(Worker::new(id, Arc::clone(&receiver), logger.clone_box()));
        }
        ThreadPool { workers, sender, logger }
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Job::new(Box::new(f), None);

        self.sender.send(Message::NewJob(job)).unwrap();
    }

    pub fn execute_req<F>(&self, f: F, req_id: RequestId)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Job::new(Box::new(f), Some(req_id));

        self.sender.send(Message::NewJob(job)).unwrap();
    }
}

impl Drop for ThreadPool{
    fn drop(&mut self) {
        
        for _ in &mut self.workers {
            match self.sender.send(Message::Terminate){
                Ok(_)=>(),
                Err(e) => eprintln!("{}",e)
            }
        }

        for worker in &mut self.workers {
            self.logger.log_info(format!("Shutting down worker {}", worker.id).as_str());
            if let Some(thread) = worker.thread.take() {
                thread.join().unwrap();
            }
        }

    }
}

#[derive(Debug)]
struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Message>>>, logger: Box<dyn ILoggerV2>) -> Worker {
        let thread = thread::spawn(move || loop {
            let msg = receiver.lock().unwrap().recv().unwrap();

            match msg{
                Message::NewJob(job) => {
                    let req_id = job.req_id.map(|id| format!(" Request Id #{id}"))
                        .unwrap_or("".to_string());

                    logger.log_info(format!("Worker {id} got a job; executing{req_id}").as_str());

                    (job.run)();

                    logger.log_info(format!("Worker {id} finished job{req_id}").as_str());
                },
                Message::Terminate =>{
                    return;
                }
            }
        });

        Worker { id, thread: Some(thread) }
    }
}