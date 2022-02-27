import random
from tkinter import Label
import torch
from torch import nn
from torch.utils.data import Dataset
from torchvision.transforms import Lambda

random.seed()

label_count = 10
feature_count = 960
batch_size = 64
learning_rate = 1e-3
dataset_size = 50000
epochs = 10
device = "cuda"

def randomize(x, label):
    diff = x % label_count
    diff = abs(diff - label)
    diff = min(diff, label_count - diff)
    diff = diff * (diff + 1) // 2 # triangular number
    diff = 1 + diff
    return 0.0 if random.randint(0, 1 + label_count) < diff else 1.0
    #return 0.0 if diff > 1 else 1.0

def random_tensor(label):
    data = [randomize(x, label) for x in range(feature_count)]
    return torch.tensor(data)

t = random_tensor(3)
print(t)
print(f"Shape of tensor: {t.shape}")
print(f"Datatype of tensor: {t.dtype}")
print(f"Device tensor is stored on: {t.device}")

class CustomImageDataset(Dataset):
    def __init__(self):
        self.len = dataset_size

    def __len__(self):
        return self.len

    def __getitem__(self, idx):
        label = idx % label_count
        t = random_tensor(label)

        #print(label)
        # Transform label to a tensor per https://pytorch.org/tutorials/beginner/basics/transforms_tutorial.html#lambda-transforms
        label = torch.zeros(label_count, dtype=torch.float).scatter(dim=0, index=torch.tensor(label), value=1)
        #print(label)
        #print(f"Datatype of label: {label.dtype}")

        return t.to(device), label.to(device)

training_data = CustomImageDataset()

from torch.utils.data import DataLoader
train_dataloader = DataLoader(training_data, batch_size=batch_size, shuffle=False)

# Use this to verify that the shapes looks correct.
train_features, train_labels = next(iter(train_dataloader))
print(f"Feature batch shape: {train_features.size()}")
print(f"Labels batch shape: {train_labels.size()}")



class NeuralNetwork(nn.Module):
    def __init__(self):
        super(NeuralNetwork, self).__init__()
        self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(feature_count, 512),
            nn.ReLU(),
            nn.Linear(512, 512),
            nn.ReLU(),
            nn.Linear(512, label_count),
        )
        self.done = False

    def forward(self, x):
        #x = self.flatten(x)
        logits = self.linear_relu_stack(x)
        return logits

model = NeuralNetwork().to(device)
print(model)

def train_loop(dataloader, model, loss_fn, optimizer):
    size = len(dataloader.dataset)
    for batch, (X, y) in enumerate(dataloader):
        # Compute prediction and loss
        pred = model(X)
        #print("pred and y are:")
        #print(pred)
        #print(y)
        loss = loss_fn(pred, y)

        # Backpropagation
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        if batch % 100 == 0:
            loss, current = loss.item(), batch * len(X)
            print(f"loss: {loss:>7f}  [{current:>5d}/{size:>5d}]")

def test_loop(dataloader, model, loss_fn):
    samples = 500
    test_loss, correct = 0, 0

    with torch.no_grad():
        #for X, y in dataloader:
        #    print(X)
        #    pred = model(X)
        #    test_loss += loss_fn(pred, y).item()
        #    #print(pred)
        #    correct += (pred.argmax(1) == y).type(torch.float).sum().item()
        for i in range(samples):
            X, label = training_data.__getitem__(i)
            logits = model(X)
            test_loss += loss_fn(logits, label)
            guess = torch.argmax(logits)
            expected = i % label_count
            if guess == expected: correct += 1

    correct /= samples
    print(f"Test Error: \n Accuracy: {(100*correct):>0.1f}%, Avg loss: {test_loss:>8f} \n")

loss_fn = nn.MSELoss() # nn.CrossEntropyLoss()
optimizer = torch.optim.SGD(model.parameters(), lr=learning_rate)

def go():
    for t in range(epochs):
        print(f"Epoch {t+1}\n-------------------------------")
        train_loop(train_dataloader, model, loss_fn, optimizer)
        #test_loop(test_dataloader, model, loss_fn)
        test_loop(train_dataloader, model, loss_fn)
    print("Done!")

go()

for i in range(100):
    label = i % label_count
    X = random_tensor(label).to(device)
    logits = model(X)
    #pred_probab = nn.Softmax(dim=1)(logits)
    #y_pred = pred_probab.argmax(1)
    #print(f"i: {i} / Predicted class: {y_pred}")
    guess = torch.argmax(logits)
    print(f"i: {label}, guess: {guess}")
