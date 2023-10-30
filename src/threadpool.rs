
use std::{thread, sync::{mpsc, Arc, Mutex}};

use crate::utils::ILoggerV2;


type Job = Box<dyn FnOnce() + Send + 'static>;
enum Message {
    NewJob(Job),
    Terminate,
}
#[derive(Debug)]
pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Message>,
    logger: Arc<dyn ILoggerV2>
}

impl ThreadPool {
    // --snip--
    pub fn new(size: usize, logger: Arc<dyn ILoggerV2>) -> ThreadPool {
        assert!(size > 0);

        let (sender, receiver) = mpsc::channel::<Message>();

        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);

        for id in 0..size {
            workers.push(Worker::new(id, Arc::clone(&receiver), logger.clone()));
        }
        ThreadPool { workers, sender, logger }
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Box::new(f);

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
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Message>>>, logger: Arc<dyn ILoggerV2>) -> Worker {
        let thread = thread::spawn(move || loop {
            let msg = receiver.lock().unwrap().recv().unwrap();

            match msg{
                Message::NewJob(job) => {
                    logger.log_info(format!("Worker {id} got a job; executing.").as_str());

                    job();
                    logger.log_info(format!("Worker {id} finished job.").as_str());
                },
                Message::Terminate =>{
                    return;
                }
            }
        });

        Worker { id, thread: Some(thread) }
    }
}