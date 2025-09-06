# 🎓 Gradex - Decentralized Exam Registry

A secure and transparent exam result management system built on Stacks blockchain.

## 🎯 Features

- ✨ Create and manage exams
- 📝 Submit and verify exam results
- 🔐 Authorized verifier system
- 👨‍🎓 Student profile management
- 🏛️ Institution-based tracking

## 🚀 Contract Functions

### Student Management
- `register-student`: Register a new student with name and institution
- `get-student-profile`: View student profile details

### Exam Management
- `create-exam`: Create a new exam with details
- `close-exam`: Close an active exam
- `get-exam`: View exam details

### Result Management
- `submit-result`: Submit exam results for a student
- `verify-result`: Verify submitted results
- `get-exam-result`: View exam results

### Admin Functions
- `add-verifier`: Add authorized result verifier
- `remove-verifier`: Remove verifier access
- `transfer-admin`: Transfer admin rights

## 🔧 Usage

1. Deploy the contract using Clarinet
2. Register students using `register-student`
3. Add verifiers using `add-verifier`
4. Create exams using `create-exam`
5. Submit and verify results using respective functions

## ⚡ Quick Start

```bash
clarinet contract call --contract-name gradex --function-name register-student --arg '"John Doe"' --arg '"University of Blockchain"'
```

## 🔒 Security

- Only admin can create exams and manage verifiers
- Only authorized verifiers can submit and verify results
- Results cannot be modified once verified

## 🤝 Contributing

Feel free to submit issues and enhancement requests!
```


