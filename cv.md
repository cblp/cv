<style>
  body {
    font-family: Arial;
    font-size: 10pt;
  }
</style>

# Yury Syrovetsky

Montenegro (UTC+1/+2) • Remote • Open to relocation \
job32@cblp.su • +382-68-216-739                     \
linkedin.com/in/cblpsu • github.com/cblp

## Professional Summary

Senior Software Engineer with deep expertise building high-performance, scalable systems. Specialized in systems programming (C/C++/Rust), functional programming (Haskell), blockchain protocols (Cardano, Solana, Stellar), GPU acceleration (CUDA), database internals, and distributed systems (CRDT).

Proven track record of delivering impactful features, fixing critical performance issues, and leading protocol design efforts at companies like Input Output Global, SQream, Kaspersky Lab, and Yandex. Strong in software architecture, performance optimization, and cross-team collaboration.

Seeking compilers/backend/distributed systems roles.

## Technical Skills

**Languages**: C/C++, Haskell, Python (expert) · Rust (proficient) · JavaScript/TypeScript, Java/Scala, Go, PHP (familiar)

**Key Areas**: Systems programming, Blockchains (Cardano, Solana, Stellar), Compilers/DSLs, GPU (CUDA), Database engines, Distributed systems (including CRDT), Kernel development, Scalable microservices

**Tools & Technologies**: ClickHouse, Linux, MongoDB, PostgreSQL, Qt, Servant, SQLite, Tokio, Yesod

## Professional Experience

**Freelance** • Remote

April 2026 – Present

- One of several freelance projects: built and publicly launched montevent.org, an event aggregator for Montenegro, end-to-end (backend and frontend) solo in 2 weeks from a standing start, repeatedly refining extraction logic as new edge cases surfaced.

  Negotiated data feeds with 40+ sources (announcement channels, cinema websites), scanning ~4,000 pages and extracting and systematizing ~1,500 events, in Haskell and Rust; used Claude for fuzzy data matching, Cloudflare for hosting, Neon (Postgres) as the cloud database, and GCP (Cloud Run Jobs, Cloud Scheduler, Artifact Registry, Secret Manager) as the runtime, deployed via GitLab CI, Docker, and Terraform.

**Senior Software Engineer** • Wallarm • Remote

December 2025 – March 2026

- Fixed security and performance-related bugs in the C/C++ core library and nginx module, improving latency by up to 5% in a mature, heavily-optimized codebase
- Built instrumentation to measure and reduce traffic costs in the core library

**Technical Lead** • Input Output Global (IOG) • Remote

May 2025 – August 2025 (3-month contract)

- Led a 2-person senior engineering team to design the Cardano Leios protocol and associated CIP
- Analyzed impact of design alternatives on core node components and ecosystem; proposed multiple protocol improvements
- Collected and synthesized community feedback to inform final design decisions
- Delivered comprehensive impact reports that enabled leadership to select the optimal protocol variant

**Senior Software Engineer** • SQream • Remote

October 2023 – September 2024

- Diagnosed and resolved complex bugs and performance bottlenecks in GPU-accelerated analytical database engine; sped up some requests from 30 hours to 30 minutes
- Fixed ~30 functional bugs in C++ core, Haskell query compiler, and CUDA kernels, resolving customer-blocking issues in a GPU-accelerated analytical database

**Senior Software Engineer** • Input Output Global (IOG) • Remote

February 2021 – May 2022

- Implemented key features for Cardano’s Alonzo hard fork (smart contracts era) in Haskell: token minting and sending via CLI
- Contributed to node, CLI, and API improvements

**Senior Software Engineer** • Yandex • Moscow, Russia

March 2020 – December 2020

- Developed cross-service file metadata synchronization for Yandex.Disk cloud storage in Java and Python, using PostgreSQL and ClickHouse storages
- Designed and prototyped CRDT-based application state synchronization framework

**Senior Software Engineer** • Kaspersky Lab • Moscow, Russia

February 2016 – March 2020

- Designed domain-specific languages and implemented security policy compiler for Kaspersky OS in Haskell (~ 50 kloc), targeted kernel-level hard real-time C code (guaranteed latency < 10 ms)
- Developed object-capability security model and other policies
- Implemented high-level logic and C code generation using Haskell
- Mentored 5 junior engineers on functional programming techniques and secure systems design

**Software Engineer** • Yandex • Moscow, Russia

December 2011 – February 2016

- Maintained and scaled Wordstat.yandex.com, a search-oriented database handling >1B records per query
- Rewrote critical microservice from Python to C++, achieving ~10× higher request throughput
- Optimized a microservice to reduce memory usage by 1.5× via internal compression with no performance regression
- Presented technical talks at internal and public Yandex meetups

## Education

Master of Science in Computer Science \
Moscow Aviation Institute             \
Graduated 2020

Master thesis: “Development of a distributed embeddable database” (focused on CRDT-based data synchronization)

## Selected Talks & Publications

- Jule 2020. “Property and fuzzy testing” at C++ Russia. cppconf-moscow.ru
- May 2019. “A purely functional approach to CRDT/RON-based distributed systems” at FPURE. fpure.events, youtu.be/2MKLWCh33wE
- September 2018. “Purely functional programming and KasperskyOS” in Information Security section at RIFTECH. tech.rif.ru
- December 2017. “CRDT — correctly replicated data in Haskell” at the functional programming conference FPCONF in collaboration with my student Nikolay Loginov. Description: fpconf.ru/2017.html, video: youtu.be/VFx0H2p3g6c

## Community Leadership

- Organized 4 offline meetups for the Russian Haskell community (from 2015)
- Administrator of the Russian Haskell community online resources (website haskellru.org, Telegram and other chats, 1600+ members)

## Open Source Projects

- Haskell implementation of RON-RDT, a CRDT-based format and a set of optimal distributed data synchronization algorithms. github.com/ff-notes/ron
- FF — a CRDT-compliant distributed note taking application. github.com/ff-notes/ff
- Multiple contributions into various open source projects.
