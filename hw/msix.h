#ifndef QEMU_MSIX_H
#define QEMU_MSIX_H

#include "qemu-common.h"
#include "pci.h"

int msix_init(PCIDevice *pdev, unsigned short nentries,
              MemoryRegion *bar,
              unsigned bar_nr, unsigned bar_size);

void msix_write_config(PCIDevice *pci_dev, uint32_t address,
                       uint32_t val, int len);

int msix_uninit(PCIDevice *d, MemoryRegion *bar);

void msix_save(PCIDevice *dev, QEMUFile *f);
void msix_load(PCIDevice *dev, QEMUFile *f);

int msix_enabled(PCIDevice *dev);
int msix_present(PCIDevice *dev);

uint32_t msix_bar_size(PCIDevice *dev);

int msix_vector_use(PCIDevice *dev, unsigned vector);
void msix_vector_unuse(PCIDevice *dev, unsigned vector);
void msix_unuse_all_vectors(PCIDevice *dev);

void msix_notify(PCIDevice *dev, unsigned vector);

void msix_reset(PCIDevice *dev);

#endif
